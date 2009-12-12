-- @+leo-ver=4-thin
-- @+node:gcross.20091211100630.1308:@thin Database.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091211100630.1309:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091211100630.1309:<< Language extensions >>
-- @nl

module Database where

-- @<< Import needed modules >>
-- @+node:gcross.20091211100630.1310:<< Import needed modules >>
import Control.Applicative.Infix
import Control.DeepSeq
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ByteString (ByteString,unpack)
import Data.ByteString.Internal
import Data.Complex
import Data.ConfigFile
import Data.UUID
import Data.Word

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.Exit
import System.IO.Unsafe
import System.Random
-- @-node:gcross.20091211100630.1310:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091211100630.1311:Enumerators
-- @+node:gcross.20091211100630.1312:getX
get1 :: (Monad m) => a -> IterAct m (Maybe a)
get1 x _ = return $ Left $ Just $! x

get2 :: (Monad m) => a -> b -> IterAct m (Maybe (a,b))
get2 x y _ = return $ Left $ Just $! (x,y)
-- @-node:gcross.20091211100630.1312:getX
-- @+node:gcross.20091211100630.1313:fetchX
fetch1 :: (Monad m) => a -> IterAct m [a]
fetch1 a accum = result' (a:accum) --'

fetch2 :: (Monad m) => a -> b -> IterAct m [(a, b)]
fetch2 a b accum = result' ((a, b):accum) --'

fetch3 :: (Monad m) => a -> b -> c -> IterAct m [(a, b, c)]
fetch3 a b c accum = result' ((a, b, c):accum) --'

fetch4 :: (Monad m) => a -> b -> c -> d -> IterAct m [(a, b, c, d)]
fetch4 a b c d accum = result' ((a, b, c, d):accum) --'
-- @-node:gcross.20091211100630.1313:fetchX
-- @-node:gcross.20091211100630.1311:Enumerators
-- @+node:gcross.20091211100630.1314:Functions
-- @+node:gcross.20091211100630.1315:makeConnection
makeConnection heading = do
    either_conn <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP "connection.cfg"
        host <- get cp "data source" "host"
        database <- get cp "data source" "database"
        user <- get cp heading "user"
        password <- get cp heading "password"
        return $ connect
            [   CAhost host
            ,   CAdbname database
            ,   CAuser user
            ,   CApassword password
            ]
    case either_conn of
        Left err -> do
            print err
            exitFailure
        Right conn -> return conn
-- @-node:gcross.20091211100630.1315:makeConnection
-- @-node:gcross.20091211100630.1314:Functions
-- @-others
-- @-node:gcross.20091211100630.1308:@thin Database.hs
-- @-leo
