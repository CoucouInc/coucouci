module Cli where

import Control.Monad
import Options.Applicative
import Data.Monoid

import Types

startParser = ServerStart
    <$> strArgument (
        value "coucouci.yaml"
        <> metavar "CONFIG"
        <> help "Config file location"
    )

cliCommand :: Parser CliCommand
cliCommand = hsubparser
    ( command "start" (info startParser (progDesc "Start the server"))
    <> command "stop" (info (pure ServerStop) (progDesc "Stop the server")))

opts :: ParserInfo CliCommand
opts = info (cliCommand <**> helper)
    (fullDesc
    <> progDesc "a DYI continuous integration server")

parseArgs :: IO CliCommand
parseArgs = execParser opts
