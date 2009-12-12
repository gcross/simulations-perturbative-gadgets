-- @+leo-ver=4-thin
-- @+node:gcross.20091211100630.1380:@thin plot-gadget-state-difference.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative.Infix
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Data.Char
import Data.Complex
import Data.Function
import Data.Maybe
import Graphics.Gnuplot.Simple
import System
import System.IO
import System.Console.GetOpt
import Text.Printf

import VMPS.Database
import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.States
import VMPS.Tensors

import Debug.Trace

dataIteratee :: (MonadIO m) =>
    Int -> Double -> String ->
    IterAct m (Maybe Double,[(Double,[(Int,String,String)])],Maybe (Int,String),[(Int,String,String)])
dataIteratee
    number_of_sites perturbation_strength state_id
    (Nothing,data_set,Nothing,line_set) =
        dataIteratee
            number_of_sites perturbation_strength state_id
            (Just perturbation_strength,data_set,Nothing,line_set)
dataIteratee
    number_of_sites perturbation_strength state_id
    (Just previous_perturbation_strength,data_set,Just (previous_number_of_sites,previous_state_id),line_set) =
        assert (perturbation_strength == previous_perturbation_strength) $
        assert (number_of_sites == previous_number_of_sites) $
            result' (Just previous_perturbation_strength,data_set,Nothing,(number_of_sites,previous_state_id,state_id):line_set)--'
dataIteratee
    number_of_sites perturbation_strength state_id
    (Just previous_perturbation_strength,data_set,Nothing,line_set)
  | abs (previous_perturbation_strength - perturbation_strength) > 1e-7
      = dataIteratee
            number_of_sites perturbation_strength state_id
            (Just perturbation_strength,((previous_perturbation_strength,line_set):data_set),Nothing,[])
  | otherwise
      = result' (Just perturbation_strength,data_set,Just (number_of_sites,state_id),line_set) -- '

main =
    getArgs
    >>=
    \(model_id:operator_type:perturbation_strength_criteria) ->
        doIt
            model_id
            (case operator_type of
                "X" -> pX
                "Y" -> pY
                "Z" -> pZ
                _ -> error $ "Unrecognized operator: " ++ operator_type
            )
            (concatMap
                (\perturbation_strength -> "abs(perturbation_strength - " ++ perturbation_strength ++ ") < 1e-7 or ")
                perturbation_strength_criteria
            )

doIt :: String -> SingleQubitOperator -> String -> IO ()
doIt model_id field_operator filter_string =
    (hPutStrLn stderr $ "> " ++ sql_statement)
    >>
    makeConnection "reader"
    >>=
    flip withSession (
        (fmap (\(Just perturbation_strength,data_set,Nothing,line_set) -> ((perturbation_strength,line_set):data_set)) $
            doQuery (sql $ sql_statement) dataIteratee (Nothing,[],Nothing,[])
        )
        >>=
        mapM (
            \(perturbation_strength,line_set) ->
                let endpoint_tensor = makeOperatorSiteTensorFromSpecification 1 1 [(1 --> 1) pI]
                in fmap ((,) perturbation_strength) $
                    mapM (
                        \(number_of_sites,state_id_1,state_id_2) -> do
                            state_1 <- fmap fromJust . fetchState $ state_id_1
                            state_2 <- fmap fromJust . fetchState $ state_id_2
                            let expectation =
                                    expectationOf $
                                        [endpoint_tensor]
                                        ++
                                        makeMagneticFieldOperatorSiteTensors field_operator (canonicalStateNumberOfSites state_1-2)
                                        ++
                                        [endpoint_tensor]
                            overlap <- liftIO . evaluate . (realPart . abs) $ (expectation state_1) - (expectation state_2)
                            return ((fromIntegral number_of_sites :: Double),overlap)
                    ) line_set
        )
    )
    >>=
    return . map (\(perturbation_strength,data_points) ->
        (PlotStyle LinesPoints (CustomStyle [LineTitle (show perturbation_strength ++ " sites"), PointType 2]),data_points)
    )
    >>=
    plotPathsStyle
        [Custom "logscale" ["xy"]
        ,Key (Just ["left","top"])
        ,Title "Difference between Overlaps versus Number of Sites"
        ,XLabel "Number of Sites"
        ,YLabel "Difference between Overlaps"
        ]
  where
    sql_statement :: String
    sql_statement = "select number_of_sites, perturbation_strength, state_id from gadget_model_simulations natural join solutions where model_id = " ++ model_id ++ " and (" ++ filter_string ++ " False) and level_number <= 1 order by perturbation_strength desc, number_of_sites desc, level_number desc;"
-- @-node:gcross.20091211100630.1380:@thin plot-gadget-state-difference.hs
-- @-leo
