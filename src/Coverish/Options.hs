{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Coverish.Options
    ( Options(..)
    , parseOptions
    , versionString
    ) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_coverish (version)
import System.FilePath.Glob (Pattern, compile, match)

import qualified Data.Text.IO as T

import Coverish.Format (Format(..))
import Coverish.Trace (Execution(..))

data Options = Options
    { oInputName :: String
    , oReadInput :: IO Text
    , oWriteOutput :: Text -> IO ()
    , oFormat :: Format
    , oFilter :: Execution -> Bool
    , oVersion :: Bool
    }

-- | An intermediate representation, as given on the commandline
data Opts = Opts
    { optsFormat :: Format
    , optsExclude :: [Pattern]
    , optsInclude :: [Pattern]
    , optsOutput :: Maybe FilePath
    , optsVersion :: Bool
    , optsInput :: Maybe FilePath
    }

parseOptions :: IO Options
parseOptions = do
    Opts{..} <- execParser $ info (parser <**> helper) fullDesc

    let exclude ex = any (`match` exPath ex) optsExclude
        include ex = any (`match` exPath ex) optsInclude

        oInputName = maybe "<stdin>" id optsInput
        oReadInput = maybe T.getContents T.readFile optsInput
        oWriteOutput = maybe T.putStrLn T.writeFile optsOutput
        oFormat = optsFormat
        oFilter ex = not (exclude ex) || include ex
        oVersion = optsVersion

    return Options{..}

parser :: Parser Opts
parser = Opts
    <$> option parseFormat
        (  long "format"
        <> short 'f'
        <> metavar "FORMAT"
        <> help "Output in the given FORMAT"
        <> value FRichText
        )
    <*> many (compile <$> strOption
        (  long "exclude"
        <> short 'e'
        <> metavar "PATTERN"
        <> help "Exclude paths matching glob PATTERN"
        ))
    <*> many (compile <$> strOption
        (  long "include"
        <> short 'i'
        <> metavar "PATTERN"
        <> help "Re-include excluded paths matching glob PATTERN"
        ))
    <*> optional (strOption
        (  long "output"
        <> short 'o'
        <> metavar "PATH"
        <> help "Output to PATH (defaults to stdout)"
        ))
    <*> switch (long "version")
    <*> optional (argument str
        (  metavar "PATH"
        <> help "Read from PATH (defaults to stdin)"
        ))

parseFormat :: ReadM Format
parseFormat = eitherReader go
  where
    go "json" = Right FJSON
    go "text" = Right FText
    go "rich" = Right FRichText
    go x = Left $ "Invalid format: " <> x <>
        ". Valid options are json, text, or rich."

versionString :: String
versionString = concat
    [ "coverish v", showVersion version
    , " (", take 7 $(gitHash), ")"
    ]
