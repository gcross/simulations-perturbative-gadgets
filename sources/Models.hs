-- @+leo-ver=4-thin
-- @+node:gcross.20091211140008.1289:@thin Models.hs
-- @@language Haskell

module Models where

-- @<< Import needed modules >>
-- @+node:gcross.20091211140008.1291:<< Import needed modules >>
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Complex
import Data.Maybe

import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.Tensors
-- @-node:gcross.20091211140008.1291:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091211140008.1292:Types
-- @+node:gcross.20091211140008.1293:Model
type Model = Double -> Int -> [OperatorSiteTensor]
-- @-node:gcross.20091211140008.1293:Model
-- @-node:gcross.20091211140008.1292:Types
-- @+node:gcross.20091211140008.1574:Models
models :: IntMap Model
models = IntMap.fromList
    -- @    @+others
    -- @+node:gcross.20091211140008.1577:#1
    [(1,\perturbation_strength ->
        makeModelWithSpecialEndpointsOperatorSiteTensors
            4
            [( 1 --> 1) pI
            ,( 1 --> 3) $ (-(perturbation_strength :+ 0)/2) *: pX
            ]
            [( 1 --> 1) pI
            ,( 2 --> 2) pI
            ,( 3 --> 3) pI
            ,( 4 --> 4) pI

            ,( 1 --> 4) pX
            ,( 1 --> 2) pZ
            ,( 3 --> 2) pX
            ]
            [( 2 --> 1) pI
            ,( 4 --> 1) $ (-(perturbation_strength :+ 0)/2) *: pX
            ]
     )
    -- @-node:gcross.20091211140008.1577:#1
    -- @+node:gcross.20091215162646.1280:#8
    ,(8,\perturbation_strength ->
        makeModelWithSpecialEndpointsOperatorSiteTensors
            8
            [( 1 --> 1) pI
            ,( 1 --> 3) $ (-(perturbation_strength :+ 0)/2) *: pX
            ,( 1 --> 6) pI
            ]
            [( 1 --> 1) pI
            ,( 2 --> 2) pI
            ,( 3 --> 3) pI
            ,( 4 --> 4) pI

            ,( 1 --> 2) pZ

            ,( 1 --> 5) pZ
            ,( 5 --> 2) $ (-1) *: pZ

            ,( 1 --> 4) pX
            ,( 3 --> 2) pX

            ,( 6 --> 7) pZ
            ,( 7 --> 7) pI
            ,( 7 --> 8) pZ
            ]
            [( 2 --> 1) pI
            ,( 4 --> 1) $ (-(perturbation_strength :+ 0)/2) *: pX
            ,( 8 --> 1) pI
            ]
     )
    -- @-node:gcross.20091215162646.1280:#8
    -- @-others
    ]
-- @-node:gcross.20091211140008.1574:Models
-- @+node:gcross.20091211140008.1575:Functions
-- @+node:gcross.20091211140008.1576:lookupModel
lookupModel :: Int -> Model
lookupModel model_id =
    fromMaybe (error $ "Model " ++ show model_id ++ " is unknown.")
    .
    flip IntMap.lookup models
    $
    model_id
-- @-node:gcross.20091211140008.1576:lookupModel
-- @-node:gcross.20091211140008.1575:Functions
-- @-others
-- @-node:gcross.20091211140008.1289:@thin Models.hs
-- @-leo
