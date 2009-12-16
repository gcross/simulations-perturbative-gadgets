-- @+leo-ver=4-thin
-- @+node:gcross.20091211140008.1719:@thin simulate-gadget.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091211140008.1720:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091211140008.1720:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091211140008.1721:<< Import needed modules >>
import Acme.Dont

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.Complex
import Data.ConfigFile
import Data.IORef
import Data.UUID

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment
import System.Posix.Clock
import System.Exit

import Text.Printf

import VMPS.Algorithms
import VMPS.Database
import VMPS.EnergyMinimizationChain
import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.States
import VMPS.Tensors

import Debug.Trace

import Models
-- @-node:gcross.20091211140008.1721:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091211140008.1722:Operator tensors
-- @+at
--  rightmost is ZX
--  
--  X on first, none on last
--  on all others, must be symmetric
--  then s = 1/2
--  
-- @-at
-- @@c

makeModelOperatorSiteTensors :: Double -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors angle =
    let s = 0.5
        op = ((cos (pi / 2 * angle) :+ 0) *: pX) + ((sin (pi / 2 * angle) :+ 0) *: pY)
    in makeModelWithSpecialEndpointsOperatorSiteTensors
        4
        [(1 --> 1) pI
        ,(1 --> 2) pZ
        ,(1 --> 4) pX
        ]
        [(1 --> 1) pI
        ,(1 --> 2) pZ
        ,(2 --> 3) ((-(1-s)) *: pX)
        ,(3 --> 4) pZ
        ,(1 --> 4) ((-s)*: op)
        ,(4 --> 4) pI
        ]
        [(2 --> 1) ((-(1-s))*: pX)
        ,(3 --> 1) pZ
        ,(4 --> 1) pI
        ]
-- @-node:gcross.20091211140008.1722:Operator tensors
-- @+node:gcross.20091211140008.1723:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20091211140008.1723:analyzeTrialEnergies
-- @+node:gcross.20091211140008.1724:main
main = do
    args <- getArgs

    let model_id, number_of_sites :: Int
        model_id = read $ args !! 0
        number_of_sites = read $ args !! 1
        perturbation_strength :: Double
        perturbation_strength = read $ args !! 2
        operator_site_tensors = lookupModel model_id perturbation_strength (number_of_sites+2)
        bandwidth_increment = 2
        initial_bandwidth = 2
        bandwidth_increase_energy_change_convergence_criterion = 1e-5
        multisweep_energy_change_convergence_criterion = 1e-5
        level_similarity_tolerance = 1e-4

    connection <- makeConnection "vmps"
    (result,connection) <-
        withContinuedSession connection $
            doQuery
                (sqlbind "select 1 from gadget_model_simulations where model_id=? and number_of_sites=? and abs(perturbation_strength-?)<1e-7;"
                         [bindP model_id
                         ,bindP number_of_sites
                         ,bindP perturbation_strength
                         ]
                )
                get1
                (Nothing :: Maybe Int)

    unless (result == Nothing) $ do
        putStrLn "This data point has already been sampled."
        exitFailure

    -- @    << Define callbacks >>
    -- @+node:gcross.20091211140008.1725:<< Define callbacks >>
    next_bandwidth_ref <- newIORef initial_bandwidth
    level_number_ref <- newIORef 1

    let getHeading = liftM (printf "LEVEL %i: ") (readIORef level_number_ref :: IO Int)
        callback_to_decide_whether_to_declare_victory_with_trial chain = do
            heading <- getHeading
            putStrLn $ heading ++ " energy = " ++ (show . chainEnergy $ chain)
            level_number <- readIORef level_number_ref
            let new_level_number = level_number + 1
            putStrLn $ printf "Now starting on level %i... (bandwidth=2 sweeps will not be displayed)" new_level_number
            writeIORef level_number_ref new_level_number
            alwaysDeclareVictory chain
        callback_to_increase_bandwidth chain = do
            next_bandwidth <- readIORef next_bandwidth_ref
            writeIORef next_bandwidth_ref (next_bandwidth+bandwidth_increment)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_after_each_sweep victory_flag latest_chain = do
            heading <- getHeading
            next_bandwidth <- readIORef next_bandwidth_ref
            let current_bandwidth = next_bandwidth-bandwidth_increment
            unless (current_bandwidth <= 2) $
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain) )
    -- @-node:gcross.20091211140008.1725:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20091211140008.1726:<< Run simulation >>
    let findTwoLevels attempt_number overlap_tensor_trios =
            (fmap unzip3 $ 
                solveForMultipleLevelsWithCallbacks
                    callback_to_decide_whether_to_declare_victory_with_trial
                    (newChainCreator
                        (writeIORef next_bandwidth_ref (initial_bandwidth+bandwidth_increment))
                        operator_site_tensors
                        2 initial_bandwidth
                    )
                    callback_to_increase_bandwidth
                    callback_after_each_sweep
                    ignoreSiteCallback
                    bandwidth_increase_energy_change_convergence_criterion
                    multisweep_energy_change_convergence_criterion
                    0
                    1000
                    2
                    overlap_tensor_trios
            ) >>= \result@([energy_1, energy_2],_,_) ->
                    if abs (energy_1 - energy_2) < level_similarity_tolerance
                        then return result
                        else putStrLn "The two levels do not agree!"
                             >>
                             if attempt_number < 3
                                then do
                                    putStrLn "Restarting simulation..."
                                    writeIORef level_number_ref 1
                                    findTwoLevels (attempt_number + 1) overlap_tensor_trios
                                else do
                                    putStrLn "Three attempts to find the levels have failed.  Giving up!"
                                    exitFailure

    ( [ground_energy_1, ground_energy_2]
     ,[ground_state_1 , ground_state_2 ]
     ,ground_states_overlap_tensor_trios
     ) <- findTwoLevels 1 []

    ( [excited_energy_1, excited_energy_2]
     ,[excited_state_1 , excited_state_2 ]
     ,_
     ) <- findTwoLevels 1 ground_states_overlap_tensor_trios

    let energies = [ground_energy_1,ground_energy_2,excited_energy_1,excited_energy_2]
    -- @-node:gcross.20091211140008.1726:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    let energy_gap = (excited_energy_1 `min` excited_energy_2) - (ground_energy_1 `max` ground_energy_2)

    putStrLn ""
    putStrLn $ "The gap is " ++ show energy_gap


    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

    -- @    << Store in database >>
    -- @+node:gcross.20091211140008.1727:<< Store in database >>
    withSession connection $
        withTransaction ReadCommitted $ do
            solution_id <-
                storeSolution
                    [(ground_energy_1,ground_state_1)
                    ,(ground_energy_2,ground_state_2)
                    ,(excited_energy_1,excited_state_1)
                    ,(excited_energy_2,excited_state_2)
                    ]
            number_of_rows_inserted <- execDML
                (cmdbind "insert into gadget_model_simulations (model_id, number_of_sites, perturbation_strength, solution_id, energy_gap, simulation_running_time) values (?,?,?,?::uuid,?,?::interval);"
                     [bindP model_id
                     ,bindP number_of_sites
                     ,bindP perturbation_strength
                     ,bindP solution_id
                     ,bindP energy_gap
                     ,bindP (show time_in_seconds ++ " seconds")
                     ]
                )
            if (number_of_rows_inserted == 1)
                then liftIO $ do
                    putStrLn ""
                    putStrLn $ "The id of the stored solution is " ++ solution_id
                    putStrLn ""
                else do
                    liftIO . putStrLn $
                        "Error adding the solution to the database. ("
                        ++ show number_of_rows_inserted ++
                        " rows inserted.)"
                    rollback
    -- @-node:gcross.20091211140008.1727:<< Store in database >>
    -- @nl
-- @-node:gcross.20091211140008.1724:main
-- @-others
-- @-node:gcross.20091211140008.1719:@thin simulate-gadget.hs
-- @-leo
