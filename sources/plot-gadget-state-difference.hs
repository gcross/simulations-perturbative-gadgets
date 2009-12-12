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

import VMPSDatabase

import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.States
import VMPS.Tensors

import Debug.Trace

dataIteratee :: (MonadIO m) =>
    Int -> Double -> String ->
    IterAct m (Maybe Int,[(Int,[(Double,String,String)])],Maybe (Double,String),[(Double,String,String)])
dataIteratee
    number_of_sites perturbation_strength state_id
    (Nothing,data_set,Nothing,line_set) =
        dataIteratee
            number_of_sites perturbation_strength state_id
            (Just number_of_sites,data_set,Nothing,line_set)
dataIteratee
    number_of_sites perturbation_strength state_id
    (Just previous_number_of_sites,data_set,Just (previous_perturbation_strength,previous_state_id),line_set) =
        assert (perturbation_strength == previous_perturbation_strength) $
        assert (number_of_sites == previous_number_of_sites) $
            result' (Just previous_number_of_sites,data_set,Nothing,(perturbation_strength,previous_state_id,state_id):line_set)--'
dataIteratee
    number_of_sites perturbation_strength state_id
    (Just previous_number_of_sites,data_set,Nothing,line_set) =
        result' (Just number_of_sites,new_data_set,Just (perturbation_strength,state_id),line_set)--'
  where
    new_data_set = if previous_number_of_sites /= number_of_sites
                        then (previous_number_of_sites,line_set):data_set
                        else data_set

main =
    getArgs
    >>=
    \(model_id:operator_type:number_of_sites_criteria) ->
        doIt
            model_id
            (case operator_type of
                "X" -> pX
                "Y" -> pY
                "Z" -> pZ
            )
            (concatMap
                (\number_of_sites -> "number_of_sites = " ++ number_of_sites ++ " or ")
                number_of_sites_criteria
            )

doIt :: String -> SingleQubitOperator -> String -> IO ()
doIt model_id field_operator filter_string =
    (hPutStrLn stderr $ "> " ++ sql_statement)
    >>
    makeConnection "reader"
    >>=
    flip withSession (
        (fmap (\(Just number_of_sites,data_set,Nothing,line_set) -> ((number_of_sites,line_set):data_set)) $
            doQuery (sql $ sql_statement) dataIteratee (Nothing,[],Nothing,[])
        )
        >>=
        mapM (
            \(number_of_sites,line_set) ->
                let endpoint_tensor = makeOperatorSiteTensorFromSpecification 1 1 [(1 --> 1) pI]
                in fmap ((,) number_of_sites) $
                    mapM (
                        \(perturbation_strength,state_id_1,state_id_2) -> do
                            state_1 <- fmap fromJust . fetchState $ state_id_1
                            state_2 <- fmap fromJust . fetchState $ state_id_2
                            let number_of_sites = canonicalStateNumberOfSites state_1
                                expectation = expectationOf $
                                                    [endpoint_tensor]
                                                    ++
                                                    makeMagneticFieldOperatorSiteTensors field_operator (number_of_sites-2)
                                                    ++
                                                    [endpoint_tensor]
                            overlap <- liftIO . evaluate . (realPart . abs) $ (expectation state_1) - (expectation state_2)
                            return (perturbation_strength,overlap)
                    ) line_set
        )
    )
    >>=
    return . map (\(number_of_sites,data_points) ->
        (PlotStyle LinesPoints (CustomStyle [LineTitle (show number_of_sites ++ " sites"), PointType 2]),data_points)
    )
    >>=
    plotPathsStyle
        [Custom "logscale" ["xy"]
        ,Key (Just ["left","top"])
        ,Title "Difference between Overlaps versus Perturbation Strength"
        ,XLabel "Perturbation Strength"
        ,YLabel "Difference between Overlaps "
        ]
  where
    sql_statement :: String
    sql_statement = "select number_of_sites, perturbation_strength, state_id from gadget_model_simulations natural join solutions where model_id = " ++ model_id ++ " and (" ++ filter_string ++ " False) and level_number <= 1 order by number_of_sites desc, perturbation_strength desc, level_number desc;"
-- @-node:gcross.20091211100630.1380:@thin plot-gadget-state-difference.hs
-- @-leo
