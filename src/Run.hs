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
import Data.ByteString.Lazy (ByteString, toStrict, fromChunks)
import qualified Data.ByteString as BS
import qualified Control.Concurrent.MSemN2 as Sem
import Conduit as C

import Types


runJob :: CiConfig -> Job -> T.Text -> IO ()
runJob config job branch = do
    let cloneUrl = "https://github.com/" <> jobName job <> ".git"
    let steps = jobSteps job
    let sem = ciConfigExecutorLock config
    let log = ciConfigLogger config
    runProcess "mkdir -p .coucouci"
    clone job cloneUrl branch
    log $ "got " ++ show (length steps) ++ " steps\n"
    results <- Async.mapConcurrently (Sem.with sem 1 . runStep log job) steps
    forM_ (zip results steps) $ \a -> log (T.unpack $ prettyResults a <> "\n")
    log $ "Done executing job " ++ T.unpack (jobName job) ++ "\n"


runStep :: (String -> IO ()) -> Job -> Step -> IO (Exit.ExitCode, ByteString, ByteString)
runStep log job step = do
    log $ "Executing command: " <> T.unpack (stepCommand step) <> "\n"
    let prefix = ".coucouci/" <> jobName job
    let cmd = shell $ T.unpack $ stepCommand step
    let log' msg = log $ T.unpack (fromMaybe (stepCommand step) (stepName step)) ++ "| " ++ msg
    let process = setStdout createSource $ setStderr createSource cmd
    withProcess process $ \p -> do
        [out, err] <- Async.mapConcurrently
            (\source -> C.runConduit $ source .| logAndAccumulate log')
            [getStdout p, getStderr p]
        exit <- waitExitCode p
        pure (exit, fromChunks out, fromChunks err)


logAndAccumulate :: (String -> IO ()) -> C.ConduitM BS.ByteString a IO [BS.ByteString]
logAndAccumulate log = C.mapMC (\s -> log (T.unpack $ T.decodeUtf8 s) >> pure s) .| C.sinkList


clone :: Job -> T.Text -> T.Text -> IO ()
clone job cloneUrl branch = do
    putStrLn $ "cloning: " <> T.unpack cloneUrl
    let target = ".coucouci/" <> jobName job
    alreadyThere <- Dir.doesDirectoryExist (T.unpack target)
    if alreadyThere
        then do
            let cmd = "git clean -f && git checkout " <> branch <> " && git pull --force"
            let process = setWorkingDir (T.unpack target) $ shell $ T.unpack cmd
            result <- runProcess process
            putStrLn $ "exit code: " ++ show result
        else do
            let cmd = "git clone -b " <> branch <> " --depth=1 " <> cloneUrl <> " " <> target
            result <- runProcess $ shell $ T.unpack cmd
            putStrLn $ "exit code: " ++ show result


prettyResults :: ((Exit.ExitCode, ByteString, ByteString), Step) -> T.Text
prettyResults ((Exit.ExitSuccess, _, _), step) =
    fromMaybe (stepCommand step) (stepName step) <> " ✓"
prettyResults ((_, out, err), step) =
  let
    name = fromMaybe (stepCommand step) (stepName step)
    header = "$ " <> name <> " ✗"
  in
    T.concat
        [ header
        , T.decodeUtf8 (toStrict out)
        , T.decodeUtf8 (toStrict err)
        ]
