-- @+leo-ver=4-thin
-- @+node:gcross.20091215162646.1343:@thin sweep-bitslayer-gadget.hs
-- @@language Haskell

import Control.Monad
import Data.Function
import Data.List
import System.IO
import System.Process
import Text.Printf

model_id :: Int
model_id = 8

system_parameters :: [[String]]
system_parameters = [
        [show model_id,show number_of_sites,show perturbation_strength]
        |   number_of_sites <- [10,20,40,80,160 :: Int]
        ,   perturbation_strength <- [0.01,0.02,0.04,0.1 :: Double]
    ]

parametersToScript :: [String] -> String
parametersToScript configuration =
    let job_name = intercalate "-" ("gadget":configuration)
    in unlines
        ["#PBS -d /home/gcross/Projects/QC/Simulations/Gadget"
        ,"#PBS -N " ++ job_name
        ,"#PBS -e logs/err/" ++ job_name
        ,"#PBS -o logs/out/" ++ job_name
        ,"#PBS -v LD_LIBRARY_PATH=/usr/local/pgsql/lib"
        ,""
        ,unwords ("programs/simulate-gadget":configuration)
        ]

main = forM_ system_parameters $ \parameters -> do
    (Just stdin,_,_,_) <- createProcess $ (shell "qsub") { std_in = CreatePipe }
    hPutStrLn stdin . parametersToScript $ parameters
    hFlush stdin
    return ()
-- @-node:gcross.20091215162646.1343:@thin sweep-bitslayer-gadget.hs
-- @-leo
