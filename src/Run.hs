module Run where

import Control.Concurrent.Async as Async
import qualified Data.Text as T
import System.Process.Typed

import Types

runJob :: Job -> IO ()
runJob job = do
    let steps = jobSteps job
    putStrLn $ "got " ++ show (length steps) ++ " steps"
    exitCodes <- Async.mapConcurrently (runProcess . shell . T.unpack . stepCommand) steps
    print exitCodes
