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
import Control.Lens
import Data.Aeson.Lens
import Data.Text

import qualified Control.Concurrent.Async as Async
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
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
    liftIO $ print $ "github payload: " ++ show githubPayload
    config <- ask
    -- liftIO $ print $ "config: " ++ show config
    let cloneUrl = githubPayload ^.key "repository" .key "clone_url" ._String
    case List.find ((== cloneUrl) . jobSource) (ciConfigJobs config) of
      Nothing -> liftIO $ print "no matching clone url, ignoring event"
      -- TODO add a MonadResource constraint to cleanup async job in case server goes down
      -- (restart or stop)
      Just jobToRun -> void $ liftIO $ Async.async $ Run.runJob config jobToRun
    return NoContent

hookServer :: CiConfig -> Server HookAPI
hookServer config = enter (readerToHandler config) hookServerT

-- hookServer :: Server HookAPI
-- hookServer githubPayload = do
--     liftIO $ print githubPayload
--     return NoContent

hookApp :: CiConfig -> Wai.Application
hookApp config = serve hookAPI (hookServer config)

runHookServer :: Int -> CiConfig -> IO ()
runHookServer port config = run port (hookApp config)
