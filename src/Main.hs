{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import System.Process.Typed
-- import Conduit as C
-- import Control.Concurrent.Async as Async
-- import Control.Concurrent
-- import qualified Data.ByteString as BS
--
--
-- main :: IO ()
-- main = do
--     let cmd = shell "for i in {1..5}; do echo $i && sleep 1; done;"
--     withProcess (setStdout createSource cmd) $ \p -> do
--         C.runConduit $ getStdout p
--             .| C.mapMC BS.putStr
--             .| C.sinkNull
--         print "process done"
--     putStrLn "done"

import Control.Monad
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Async as Async
import System.Exit (die)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Control.Concurrent.MSemN2 as Sem
import qualified Control.Concurrent.MVar as MVar

import System.Process.Typed

import Types
import Hook
import qualified Run as Run

import Data.Aeson.Lens
import Control.Lens


parseConfig :: BS.ByteString -> Either Yaml.ParseException Config
parseConfig = Yaml.decodeEither'

main :: IO ()
main = testConfig

makeCiConfig :: Config -> IO CiConfig
makeCiConfig conf = do
    sem <- Sem.new (serverExecutorCount $ configServer conf)
    loggerLock <- MVar.newMVar ()
    let logger msg = MVar.withMVar loggerLock (\_ -> putStr msg)
    pure CiConfig
        { ciConfigExecutorLock = sem
        , ciConfigJobs = configJobs conf
        , ciConfigLogger = logger
        }


testConfig :: IO ()
testConfig = do
    raw <- BS.readFile "coucouci.yaml"
    case parseConfig raw of
        Left err -> die (show err)
        Right config -> do
            putStrLn $ "got config: " ++ show config
            ciConf <- makeCiConfig config
            mapM_ (Run.runJob ciConf) (configJobs config)
