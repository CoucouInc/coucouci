{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hook (runHookServer) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import Control.Lens
import Data.Aeson.Lens
import Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe

import qualified Control.Concurrent.Async as Async
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Void
import qualified Data.List as List

import qualified Run as Run
import Types

type HookAPI = "coucouci" :> ReqBody '[JSON] GithubPayload :> Post '[JSON] NoContent

type Resp = Text

hookAPI :: Proxy HookAPI
hookAPI = Proxy

readerToHandler' :: forall a. CiConfig -> ReaderT CiConfig IO a -> Handler a
readerToHandler' config r = liftIO (runReaderT r config)

readerToHandler :: CiConfig -> ReaderT CiConfig IO :~> Handler
readerToHandler config = NT $ readerToHandler' config

hookServerT :: ServerT HookAPI (ReaderT CiConfig IO)
hookServerT githubPayload = do
    config <- ask
    let raw = unpack $ toStrict $ JSON.encodeToLazyText githubPayload :: String
    liftIO $ ciConfigLogger config $ "github payload: \n" ++ raw ++ "\n"
    let repoName = githubPayload ^.key "repository" .key "full_name" ._String

    let inspected = runExcept $ do
            job <- except $ hoist "No matching job" $
                List.find ((== repoName) . jobName) (ciConfigJobs config)
            branch <- except $ hoist "no branch found!" $ getBranch githubPayload
            let branchFound = case jobBranches job of
                    AllBranches -> True
                    SomeBranches brs -> isJust $ List.find (== branch) brs
            unless branchFound $ throwError "no matching branch"
            pure (job, branch)
    case inspected of
        Left msg -> liftIO $ ciConfigLogger config msg
        Right (job, branch) -> void $ liftIO $ Async.async $ Run.runJob config job branch

    -- case List.find ((== repoName) . jobName) (ciConfigJobs config) of
    --     Nothing -> liftIO $ ciConfigLogger config $ "No matching repo for " ++ T.unpack repoName
    --     Just job -> do
    --         let cloneUrl = githubPayload ^.key "repository" .key "clone_url" ._String

    -- liftIO $ print $ "config: " ++ show config
    -- case List.find ((== cloneUrl) . jobSource) (ciConfigJobs config) of
    --   Nothing -> liftIO $ print "no matching clone url, ignoring event"
    --   -- TODO add a MonadResource constraint to cleanup async job in case server goes down
    --   -- (restart or stop)
    --   Just jobToRun -> void $ liftIO $ Async.async $ Run.runJob config jobToRun
    return NoContent

hookServer :: CiConfig -> Server HookAPI
hookServer config = enter (readerToHandler config) hookServerT

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

testExcept :: String -> Either String Int
testExcept str = runExcept $ do
    when (str == "foo") (throwError "boom")
    stuff <- except $ hoist "bar" $ List.find (const True) [str]
    -- len <-if str == "foo"
    --         then throwError "boom"
    --         else pure (Prelude.length str)
    let len = Prelude.length str
    if len < 3
        then throwError "too small"
        else pure len
