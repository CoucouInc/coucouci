{-# LANGUAGE OverloadedStrings #-}

module Run where

import Data.Monoid
import Control.Monad
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import Control.Concurrent.Async as Async
import qualified Data.Text as T
import System.Process.Typed

import Types

runJob :: Job -> IO ()
runJob job = do
    let steps = jobSteps job
    runProcess "mkdir -p .coucouci"
    clone job
    putStrLn $ "got " ++ show (length steps) ++ " steps"
    exitCodes <- mapM (runStep job) steps
    print exitCodes

runStep :: Job -> Step -> IO Exit.ExitCode
runStep job step = do
    let url = jobSource job
    putStrLn $ "Executing command: " <> T.unpack (stepCommand step)
    let prefix = ".coucouci/" <> jobName job
    runProcess $ setWorkingDir (T.unpack prefix) $ shell (T.unpack $ stepCommand step)

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

testCWD :: IO ()
testCWD = do
    runProcess "mkdir -p .coucouci"
    runProcess "cd .coucouci"
    runProcess "ls" >>= print
