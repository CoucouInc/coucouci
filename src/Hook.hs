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

-- hookServerT :: ServerT HookAPI (Reader Config)
-- hookServerT =

readerToHandler' :: forall a. Config -> ReaderT Config IO a -> Handler a
readerToHandler' config r = liftIO (runReaderT r config)

readerToHandler :: Config -> ReaderT Config IO :~> Handler
readerToHandler config = NT $ readerToHandler' config

hookServerT :: ServerT HookAPI (ReaderT Config IO)
hookServerT githubPayload = do
    liftIO $ print $ "github payload: " ++ show githubPayload
    config <- ask
    liftIO $ print $ "config: " ++ show config
    let cloneUrl = githubPayload ^.key "repository" .key "clone_url" ._String
    case List.find ((== cloneUrl) . jobSource) (configJobs config) of
      Nothing -> liftIO $ print "no matching clone url, ignoring event"
      -- TODO add a MonadResource constraint to cleanup async job in case server goes down
      -- (restart or stop)
      Just jobToRun -> void $ liftIO $ Async.async $ Run.runJob jobToRun
    return NoContent

hookServer :: Config -> Server HookAPI
hookServer config = enter (readerToHandler config) hookServerT

-- hookServer :: Server HookAPI
-- hookServer githubPayload = do
--     liftIO $ print githubPayload
--     return NoContent

hookApp :: Config -> Wai.Application
hookApp config = serve hookAPI (hookServer config)

runHookServer :: Config -> IO ()
runHookServer config = run (serverPort $ configServer config) (hookApp config)
