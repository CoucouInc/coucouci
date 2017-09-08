{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import GHC.Generics
import qualified Data.HashMap.Strict as Map
import Data.Yaml as Yaml
import Data.Text (Text, unpack)
import qualified Data.Char as Char
import Data.Aeson.Types as JSON

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

-- instance FromJSON ConfigServer where
--     parseJSON = error "wip parsejson configserver"

data Job = Job
    { jobName :: !Text
    , jobSource :: !Text
    , jobSteps :: [Step]
    } deriving (Show)

parseJob :: Text -> Yaml.Value -> Parser Job
parseJob jobName (Object o) = do
    jobSource <- o .: "source"
    jobSteps <- o .: "steps"
    pure Job{..}
parseJob jobName _ = fail $ "cannot parse job: " ++ unpack jobName

data Step = RunStep
    { stepName :: Maybe Text
    , stepCommand :: !Text
    } deriving (Show)

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
