{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Hook (runHookServer) where

import GHC.Generics
import Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe

import Control.Concurrent.STM.TVar
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.WebSocket
import Network.WebSockets as WS
import Data.Void
import qualified Data.List as List
import qualified Data.HashMap.Strict as Map
import Data.ByteString (ByteString)
import Control.Exception as Exc hiding (Handler)

import qualified Run as Run
import Types


newtype CoucouHandler a = CoucouHandler
    { runCoucouHandler :: ReaderT CiConfig (ExceptT ServantErr IO) a }
    deriving (Functor, Applicative, Monad, MonadReader CiConfig, MonadIO, MonadError ServantErr)


newtype RunResult = RunResult T.Text deriving (Show)
instance JSON.ToJSON RunResult where
    toJSON (RunResult txt) = JSON.object ["result" .= txt]

type CoucouAPI = "run" :> Capture "jobName" Text :> Post '[JSON] RunResult
    :<|> "watch" :> Get '[JSON] NoContent

type HookAPI =
    "coucouci" :> ReqBody '[JSON] GithubPayload :> Post '[JSON] NoContent
    :<|> "coucouci" :> "api" :> CoucouAPI

type Resp = Text

type EchoAPI = "echo" :> WebSocket


hookAPI :: Proxy (HookAPI :<|> EchoAPI)
hookAPI = Proxy


readerToHandler' :: forall a. CiConfig -> CoucouHandler a -> Handler a
readerToHandler' config r = do
    result <- liftIO $ runExceptT (runReaderT (runCoucouHandler r) config)
    case result of
        Left err -> throwError err
        Right stuff -> pure stuff


readerToHandler :: CiConfig -> CoucouHandler :~> Handler
readerToHandler config = NT $ readerToHandler' config


hookHandler :: JSON.Value -> CoucouHandler NoContent
hookHandler githubPayload = do
    config <- ask
    let raw = unpack $ toStrict $ JSON.encodeToLazyText githubPayload :: String
    -- liftIO $ ciConfigLogger config $ "github payload: \n" ++ raw ++ "\n"
    let repoName = githubPayload ^.key "repository" .key "full_name" ._String

    let inspected = runExcept $ do
            toRun@(job, jobDetails) <- except $ hoist ("No job matching " ++ T.unpack repoName) $
                findJob repoName (ciConfigJobs config)
            branch <- except $ hoist "no branch found!" $ getBranch githubPayload
            let branchFound = case jobBranches job of
                    AllBranches -> True
                    SomeBranches brs -> isJust $ List.find (== branch) brs
            unless branchFound $ throwError "no matching branch"
            pure (toRun, branch)
    case inspected of
        Left msg -> liftIO $ ciConfigLogger config (msg ++ "\n")
        Right (toRun, branch) -> void $ liftIO $ Async.async $ Run.runJob config toRun branch

    return NoContent


-- figure out the correct types to allow throwing an error in the handlers
runJobApiHandler :: Text -> CoucouHandler RunResult
runJobApiHandler jobName = do
    ciConfig <- ask
    case Map.lookup jobName (ciConfigJobs ciConfig) of
      Nothing -> do
          liftIO $ print $ Map.keys (ciConfigJobs ciConfig)
          throwError err404 {errBody = "Job not found: " <> LBS.fromStrict (T.encodeUtf8 jobName)}
      Just j -> do
          liftIO $ void $ Async.async $ Run.runJob ciConfig j "master"
          liftIO $ ciConfigLogger ciConfig "returning \"started\" here\n"
          pure (RunResult "started")



apiHandler :: (Text -> CoucouHandler RunResult) :<|> CoucouHandler NoContent
apiHandler = runJobApiHandler :<|> pure NoContent


wsHandler :: CiConfig -> Connection -> Handler ()
wsHandler config conn = do
    liftIO $ ciConfigLogger config "ws handler here\n"
    let jobName = "looping"
    case Map.lookup jobName (ciConfigJobs config) of
      Nothing ->
          -- todo this doesn't quite work properly, need a helper to send the message
          -- through the ws connection and then close it gracefully
          throwError err404 {errBody = "Job not found: " <> LBS.fromStrict (T.encodeUtf8 jobName)}
      Just (_, tJobDetail) -> do
          jobDetail <- liftIO $ readTVarIO tJobDetail
          let status = "status: " <> pack (show $ jobDetail ^. jobDetailStatus)
          liftIO $ WS.sendTextData conn status
          pure ()


hookServerT :: ServerT HookAPI CoucouHandler
hookServerT = hookHandler :<|> apiHandler

hookServer :: CiConfig -> Server (HookAPI :<|> EchoAPI)
hookServer config = enter (readerToHandler config) hookServerT :<|> wsHandler config

hookApp :: CiConfig -> Wai.Application
hookApp config = serve hookAPI (hookServer config)

runHookServer :: Int -> CiConfig -> IO ()
runHookServer port config = run port (hookApp config)


testBranch :: IO ()
testBranch = do
    raw <- LBS.readFile "github.json"
    case JSON.eitherDecode' raw of
        Left err -> putStrLn err
        Right (json :: JSON.Value) -> do
            let repoName = json ^.key "repository" .key "full_name" ._String
            putStrLn $ "repo name: " ++ unpack repoName
            putStrLn $ "branch: " ++ unpack (fromMaybe "" $ getBranch json)
            putStrLn "do things here"


getBranch :: JSON.Value -> Maybe Text
getBranch githubEvent = do
    branch <- githubEvent ^?key "ref" ._String
    case split (=='/') branch of
        [] -> Nothing
        xs -> Just $ List.last xs


hoist :: a -> Maybe b -> Either a b
hoist x Nothing = Left x
hoist _ (Just x) = Right x

except :: Either e a -> Except e a
except m = ExceptT (Identity m)


findJob :: Text -> Map.HashMap Text (Job, TVar JobDetail) -> Maybe (Job, TVar JobDetail)
findJob gitubName = List.find (\(j, _) -> jobGithubName j == gitubName) . fmap snd . Map.toList
