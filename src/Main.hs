{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Async as Async
import System.Exit (die)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Control.Concurrent.MSemN2 as Sem
import qualified Control.Concurrent.MVar as MVar
import qualified System.IO as IO

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
    cmd <- Cli.parseArgs
    case cmd of
      ServerStart configLocation -> startServer configLocation
      ServerStop -> putStrLn "stopping server"
      ServerStatus -> die "not implemented yet"
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
            Run.runJob ciConf (head $ configJobs config) (T.pack "branch2")
            -- mapM_ (Run.runJob ciConf) (configJobs config)
