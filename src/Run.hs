{-# LANGUAGE OverloadedStrings #-}

module Run where

import Data.Monoid
import Control.Monad
import Data.Maybe
import Data.Foldable
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import Control.Concurrent.Async as Async
import qualified Control.Concurrent.MSemN2 as Sem
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TMChan as Chan
import qualified Data.Text as T
import Data.Text.Encoding as T
import System.Process.Typed
import Data.ByteString.Lazy (ByteString, toStrict, fromChunks)
import qualified Data.ByteString as BS
import Conduit as C
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as Map
import Control.Lens

import Types
import MemoryChan


runJob :: CiConfig -> (Job, STM.TVar JobDetail) -> T.Text -> IO ()
runJob config (job, jobDetail) branch = do
    let cloneUrl = "https://github.com/" <> jobGithubName job <> ".git"
    let steps = jobSteps job
    let sem = ciConfigExecutorLock config
    let log = ciConfigLogger config
    runProcess "mkdir -p .coucouci"
    clone job cloneUrl branch
    log $ "got " ++ show (length steps) ++ " steps\n"
    let stepPrefix = ".coucouci/" <> jobName job <> "-" <> branch

    -- reset the job status
    stepsList <- mapM makeStepProgress steps
    let stepsMap = Map.fromList stepsList
    STM.atomically $ STM.writeTVar jobDetail (JobDetail Running stepsMap)

    -- start all steps
    results <- Async.mapConcurrently (Sem.with sem 1 . runStep log stepPrefix) stepsList
    STM.atomically $ STM.modifyTVar' jobDetail (set jobDetailStatus Completed)
    forM_ (zip results steps) $ \a -> log (T.unpack $ prettyResults a <> "\n")
    log $ "Done executing job " ++ T.unpack (jobName job) ++ "\n"


makeStepProgress :: Step -> IO (Step, StepProgress)
makeStepProgress step = do
    outChan <- newMemoryChan
    errChan <- newMemoryChan
    status <- STM.newTVarIO StepNotRunning
    pure (step, StepProgress status outChan errChan)


runStep
    :: (String -> IO ())
    -> T.Text
    -> (Step, StepProgress)
    -> IO (Exit.ExitCode, ByteString, ByteString)
runStep log prefix (step, stepProgress) = do
    log $ T.unpack $ "Executing command: " <> stepCommand step <> " with prefix: " <> prefix <> "\n"
    let cmd = shell $ T.unpack $ stepCommand step
    let log' msg = log $ T.unpack (fromMaybe (stepCommand step) (stepName step)) ++ " | " ++ msg
    let process = setStdout createSource $ setStderr createSource cmd
    let outChan = stepProgress ^. stepProgressStdout
    let errChan = stepProgress ^. stepProgressStderr
    withProcess process $ \p -> do
        STM.atomically $ STM.writeTVar (stepProgress ^. stepProgressStatus) StepRunning
        Async.mapConcurrently
            (\(source, chan) -> do
                let updateFn = STM.atomically . writeMemoryChan chan
                C.runConduit $ source
                    .| C.mapMC (\s -> STM.atomically (writeMemoryChan chan s) >> pure s)
                    .| C.sinkNull
                )
            [(getStdout p, outChan), (getStderr p, errChan)]
        exit <- waitExitCode p
        let stepStatus = case exit of
                Exit.ExitSuccess -> StepExitOk
                Exit.ExitFailure code -> StepExitError code
        out' <- STM.atomically $ do
            closeMemoryChan outChan
            content <- dump outChan
            pure $ fromChunks $ toList content
        err' <- STM.atomically $ do
            closeMemoryChan errChan
            content <- dump errChan
            pure $ fromChunks $ toList content
        pure (exit, out', err')


-- TODO this throws an exception in case an error occur, need to handle that
clone :: Job -> T.Text -> T.Text -> IO ()
clone job cloneUrl branch = do
    putStrLn $ "cloning: " <> T.unpack cloneUrl
    let target = ".coucouci/" <> jobName job <> "-" <> branch
    alreadyThere <- Dir.doesDirectoryExist (T.unpack target)
    if alreadyThere
        then do
            let cmd = "git clean -f && git checkout " <> branch <> " && git pull --force"
            let process = setWorkingDir (T.unpack target) $ shell $ T.unpack cmd
            withProcess process checkExitCode
        else do
            let cmd = "git clone -b " <> branch <> " --depth=1 " <> cloneUrl <> " " <> target
            withProcess (shell $ T.unpack cmd) checkExitCode


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
