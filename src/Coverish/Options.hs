{-# LANGUAGE TemplateHaskell #-}

module Coverish.Options
    ( Options(..)
    , parseOptions
    , versionString
    ) where

import Prelude

import Coverish.Format (Format(..))
import Coverish.Trace (Execution(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_coverish (version)
import System.FilePath.Glob (Pattern, compile, match)

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
    Opts {..} <- execParser $ info (parser <**> helper) fullDesc

    let exclude ex = any (`match` exPath ex) optsExclude
        include ex = any (`match` exPath ex) optsInclude

        oInputName = fromMaybe "<stdin>" optsInput
        oReadInput = maybe T.getContents T.readFile optsInput
        oWriteOutput = maybe T.putStrLn T.writeFile optsOutput
        oFormat = optsFormat
        oFilter ex = not (exclude ex) || include ex
        oVersion = optsVersion

    return Options { .. }


-- brittany-disable-next-binding

parser :: Parser Opts
parser = Opts
    <$> option (eitherReader readFormat)
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

readFormat :: String -> Either String Format
readFormat = \case
    "json" -> Right FJSON
    "text" -> Right FText
    "rich" -> Right FRichText
    x ->
        Left
            $ "Invalid format: "
            <> x
            <> ". Valid options are json, text, or rich."

versionString :: String
versionString =
    concat ["coverish v", showVersion version, " (", take 7 $(gitHash), ")"]
