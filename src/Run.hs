{-# LANGUAGE OverloadedStrings #-}

module Run where

import Data.Monoid
import Control.Monad
import Data.Maybe
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import Control.Concurrent.Async as Async
import qualified Data.Text as T
import Data.Text.Encoding as T
import System.Process.Typed
import Data.ByteString.Lazy (ByteString, toStrict)

import Types

runJob :: Job -> IO ()
runJob job = do
    let steps = jobSteps job
    runProcess "mkdir -p .coucouci"
    clone job
    putStrLn $ "got " ++ show (length steps) ++ " steps"
    results <- Async.mapConcurrently (runStep job) steps
    forM_ (zip results steps) $ \a -> putStrLn (T.unpack $ prettyResults a)

runStep :: Job -> Step -> IO (Exit.ExitCode, ByteString, ByteString)
runStep job step = do
    let url = jobSource job
    putStrLn $ "Executing command: " <> T.unpack (stepCommand step)
    let prefix = ".coucouci/" <> jobName job
    readProcess
        $ setWorkingDir (T.unpack prefix)
        $ setStdout byteStringOutput
        $ setStderr byteStringOutput
        $ shell (T.unpack $ stepCommand step)

clone :: Job -> IO ()
clone job = do
    putStrLn $ "cloning: " <> T.unpack (jobSource job)
    let target = ".coucouci/" <> jobName job
    alreadyThere <- Dir.doesDirectoryExist (T.unpack target)
    if alreadyThere
        then do
            let process = setWorkingDir (T.unpack target) $ shell "git pull --force"
            result <- runProcess process
            putStrLn $ "exit code: " ++ show result
        else do
            let cmd = "git clone --depth=1 " <> jobSource job <> " " <> target
            result <- runProcess $ shell $ T.unpack cmd
            putStrLn $ "exit code: " ++ show result

prettyResults :: ((Exit.ExitCode, ByteString, ByteString), Step) -> T.Text
prettyResults ((exitCode, out, err), step) =
  let
    name = fromMaybe (stepCommand step) (stepName step)
    mark = if exitCode == Exit.ExitSuccess then "✓" else "✗"
    header = "$ " <> name
    footer = name <> " " <> mark
  in
    T.unlines
        [ header
        , T.decodeUtf8 (toStrict out)
        , T.decodeUtf8 (toStrict err)
        , footer
        ]
