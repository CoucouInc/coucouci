{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Async as Async
import System.Exit (die)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Control.Concurrent.MSemN2 as Sem
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified System.IO as IO
import qualified Data.HashMap.Strict as Map

import System.Process.Typed

import Types
import Hook
import qualified Run
import qualified Cli

import Data.Aeson.Lens
import Control.Lens


parseConfig :: BS.ByteString -> Either Yaml.ParseException Config
parseConfig = Yaml.decodeEither'

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    cmd <- Cli.parseArgs
    case cmd of
      ServerStart c -> startServer c
      ServerStop -> putStrLn "stopping server"
      ServerStatus -> die "not implemented yet"
      Build c -> buildOnce c
    putStrLn "done"


startServer :: FilePath -> IO ()
startServer configLocation = do
    raw <- BS.readFile configLocation
    case parseConfig raw of
        Left err -> die (show err)
        Right config -> do
            putStrLn $ "got config: " ++ show config
            ciConf <- makeCiConfig config
            Hook.runHookServer (serverPort $ configServer config) ciConf

buildOnce :: FilePath -> IO ()
buildOnce configLoc = do
    raw <- BS.readFile configLoc
    case parseConfig raw of
        Left err -> die (show err)
        Right config -> do
            putStrLn $ "got config: " ++ show config
            ciConf <- makeCiConfig config
            die $ "not implemented: build once"


makeCiConfig :: Config -> IO CiConfig
makeCiConfig conf = do
    sem <- Sem.new (serverExecutorCount $ configServer conf)
    loggerLock <- MVar.newMVar ()
    let logger msg = MVar.withMVar loggerLock (\_ -> putStr msg)
    jobs <- mapM jobDetail (configJobs conf)
    pure CiConfig
        { ciConfigExecutorLock = sem
        , ciConfigJobs = Map.fromList jobs
        , ciConfigLogger = logger
        }
  where
    jobDetail j = do
        tvar <- STM.newTVarIO (JobDetail NeverRun Map.empty)
        pure (jobName j, (j, tvar))


testConfig :: IO ()
testConfig = do
    raw <- BS.readFile "coucouci.yaml"
    case parseConfig raw of
        Left err -> die (show err)
        Right config -> do
            putStrLn $ "got config: " ++ show config
            ciConf <- makeCiConfig config
            tvar <- STM.newTVarIO (JobDetail NeverRun Map.empty)
            let j = (head $ configJobs config, tvar)
            Run.runJob ciConf j (T.pack "branch2")
