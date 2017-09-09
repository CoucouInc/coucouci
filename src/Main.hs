{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Async as Async
import System.Exit (die)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T

import System.Process.Typed

import Types
import Hook
import qualified Run as Run

import Data.Aeson.Lens
import Control.Lens

parseConfig :: BS.ByteString -> Either Yaml.ParseException Config
parseConfig = Yaml.decodeEither'

main :: IO ()
main = testConfig

testConfig :: IO ()
testConfig = do
    raw <- BS.readFile "coucouci.yaml"
    case parseConfig raw of
        Left err -> die (show err)
        Right config -> do
            putStrLn $ "got config: " ++ show config
            Run.runJob $ head (configJobs config)
