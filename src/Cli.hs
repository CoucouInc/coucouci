module Cli where

import Control.Monad
import Options.Applicative
import Data.Monoid

import Types

parseConfig :: (FilePath -> a) -> Parser a
parseConfig f = f
    <$> strArgument (
        value "coucouci.yaml"
        <> metavar "CONFIG"
        <> help "Config file location"
    )

cliCommand :: Parser CliCommand
cliCommand = hsubparser
    ( command "start" (info (parseConfig ServerStart) (progDesc "Start the server"))
    <> command "stop" (info (pure ServerStop) (progDesc "Stop the server"))
    <> command "run" (info (parseConfig Build) (progDesc "Build task"))
    )

opts :: ParserInfo CliCommand
opts = info (cliCommand <**> helper)
    (fullDesc
    <> progDesc "a DYI continuous integration server")

parseArgs :: IO CliCommand
parseArgs = execParser opts
