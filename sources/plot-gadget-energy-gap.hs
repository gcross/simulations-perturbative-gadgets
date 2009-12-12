-- @+leo-ver=4-thin
-- @+node:gcross.20091211140008.1287:@thin plot-gadget-energy-gap.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Data.Char
import Graphics.Gnuplot.Simple
import System
import System.IO
import System.Console.GetOpt
import Text.Printf

import VMPS.Database

-- @<< Options handling >>
-- @+node:gcross.20091211140008.1288:<< Options handling >>
options :: [OptDescr String]
options =
 [ Option ['m']     ["minimum-bound"] -- '
     (ReqArg (\ n -> " and perturbation_strength > " ++ n)
             "#")
     "minimum bound on the perturbation strength"
 , Option ['M']     ["maximum-bound"] -- '
     (ReqArg (\ n -> " and perturbation_strength < " ++ n)
             "#")
     "maximum bound on the perturbation strength"
 ]

defaultOptions = "true"

parseOptions :: [String] -> (String,[String])
parseOptions args = 
    case getOpt Permute options args of
        (_,[],errs) -> error (concat errs ++ usageInfo header options)
        (o,number_of_sites_list,[]  ) -> (concat o,number_of_sites_list)
        (_,_,errs) -> error (concat errs ++ usageInfo header options)
    where header = "\n\nUsage: PlotGapVersusPerturbation [-m #] [-M #] <number of sites> <number of sites> ..."
-- @-node:gcross.20091211140008.1288:<< Options handling >>
-- @nl

main =
    getArgs
    >>=
    \(model_id:options) ->
        uncurry (doIt model_id) . parseOptions $ options

doIt model_id filter_string number_of_sites_list =
    makeConnection "reader"
    >>=
    flip withSession (
        forM number_of_sites_list $ \number_of_sites ->
            let sql_statement = "select perturbation_strength, energy_gap from gadget_model_simulations where number_of_sites = " ++ number_of_sites ++ filter_string ++ " and model_id=" ++ model_id ++ " order by perturbation_strength desc;"
            in do
                liftIO . hPutStrLn stderr $ "> " ++ sql_statement
                (data_points :: [(Float,Float)]) <- doQuery (sql $ sql_statement) fetch2 []
                return (number_of_sites,data_points)
    )
    >>=
    return . concatMap (\(color,(number_of_sites,data_points)) ->
        [ (PlotStyle LinesPoints (CustomStyle [LineTitle number_of_sites, LineType color, PointType 2]),data_points)
        -- , (PlotStyle Lines (CustomStyle [LineTitle ("energy gap for " ++ number_of_sites ++ " sites"), LineType color]),map (\(x,_,_) -> (x,x * x * (read number_of_sites) / 2.0)) data_points)
        -- , (PlotStyle LinesPoints (CustomStyle [LineTitle ("infidelity for " ++ number_of_sites ++ " sites"), LineType color, PointType 6]),[(x,y) | (x,_,y) <- data_points])
        ]
    ) . zip [1..]
    >>=
    plotPathsStyle
        [Custom "logscale" ["xy"]
        ,Key (Just ["left","top"])
        ,Title "Energy Gap (with predictions) & Infidelity vs. Perturbation Strength"
        ,XLabel "Perturbation Strength"
        ,YLabel "Energy Gap (Delta = 1) & Infidelity"
        ]
-- @-node:gcross.20091211140008.1287:@thin plot-gadget-energy-gap.hs
-- @-leo
