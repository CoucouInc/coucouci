{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import GHC.Generics
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Data.Yaml as Yaml
import Data.Text (Text, unpack)
import qualified Data.Char as Char
import Data.Aeson.Types as JSON
import qualified Control.Concurrent.MSemN2 as Sem
import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Reader
import qualified Data.Vector as V
import Control.Concurrent.STM (TVar)
import Data.Hashable
import Control.Lens


minimize :: String -> String
minimize "" = ""
minimize (c:str) = Char.toLower c : str

data Config = Config
    { configServer :: ConfigServer
    , configJobs :: [Job]
    }
    deriving (Show, Generic)

instance FromJSON Config where
    parseJSON = withObject "cannot parse config file" $ \o -> do
        rawJobs <- o .: "jobs"
        configServer <- o .: "config"
        case rawJobs of
          (Yaml.Object o2) -> do
            jobs <- Map.elems <$> Map.traverseWithKey parseJob o2
            pure $ Config configServer jobs
          _ -> fail "Cannot parse jobs"

data ConfigServer = ConfigServer
    { serverPort :: !Int
    , serverExecutorCount :: !Int
    } deriving (Show, Generic)

instance FromJSON ConfigServer where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = minimize . drop 6}

data Job = Job
    { jobName :: !Text
    , jobGithubName :: !Text
    , jobBranches :: JobBranch
    , jobSteps :: [Step]
    } deriving (Show)

data JobBranch = AllBranches | SomeBranches [Text] deriving Show

parseJob :: Text -> Yaml.Value -> Parser Job
parseJob jobName (Object o) = do
    jobGithubName <- o .: "githubName"
    jobBranches <- (o .:? "branches" .!= Yaml.String "master") >>= parseJobBranches
    jobSteps <- o .: "steps"
    pure Job{..}
parseJob jobName _ = fail $ "cannot parse job: " ++ unpack jobName

parseJobBranches :: Yaml.Value -> Parser JobBranch
parseJobBranches (Yaml.String str) =
    if str == "all"
        then pure AllBranches
        else pure $ SomeBranches [str]
parseJobBranches (Yaml.Array arr) = do
    branches <- traverse (withText "branch name must be a string" pure) arr
    pure $ SomeBranches (V.toList branches)
parseJobBranches _ = fail "Cannot parse branches"


data Step = RunStep
    { stepName :: Maybe Text
    , stepCommand :: !Text
    } deriving (Show, Eq, Generic)

instance Hashable Step

instance FromJSON Step where
    parseJSON = withObject "cannot parse step" $ \o -> do
        run <- o .: "run"
        case run of
          (Yaml.String str) -> pure $ RunStep Nothing str
          (Yaml.Object o) -> RunStep
              <$> o .:? "name"
              <*> o .: "command"
          _ -> fail "cannot parse step"


type GithubPayload = JSON.Value

data JobStatus = NeverRun | Running | Completed deriving Show

data JobDetail = JobDetail
    { _jobDetailStatus :: JobStatus
    , _jobDetailStepsProgress :: Map.HashMap Step (TVar StepProgress)
    }

data StepStatus = StepNotRunning | StepRunning | StepExitOk | StepExitError Int
    deriving Show

data StepProgress = StepProgress
    { _stepProgressStatus :: StepStatus
    , _stepProgressStdout :: Seq ByteString
    , _stepProgressStderr :: Seq ByteString
    } deriving Show

makeLenses ''JobDetail
makeLenses ''StepProgress


----------------------------------------
--  CLI
----------------------------------------

data CiConfig = CiConfig
    { ciConfigExecutorLock :: Sem.MSemN Int
    , ciConfigJobs :: Map.HashMap Text (Job, TVar JobDetail)
    , ciConfigLogger :: String -> IO ()
    }

data CliCommand =
    ServerStart {configLocation :: !FilePath}
    | ServerStop
    | ServerStatus deriving Show
