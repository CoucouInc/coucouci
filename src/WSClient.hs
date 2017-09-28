{-# LANGUAGE OverloadedStrings #-}

module WSClient where

-- import Data.Text
-- import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.WebSockets as WS

testClient :: IO ()
testClient = runClient "localhost" 6666 "/echo" clientApp

clientApp :: Connection -> IO ()
clientApp conn = do
    sendTextData conn ("{\"hello\": \"bar\"}" :: Text)
    response <- receiveData conn
    BS8.putStrLn response
    putStrLn "done"
