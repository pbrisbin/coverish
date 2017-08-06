{-# LANGUAGE OverloadedStrings #-}
module Coverish.SourceFile
    ( SourceFile(..)
    , LineCoverage(..)
    , sourceFiles
    ) where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.Text (Text)
import System.FilePath ((</>), splitFileName)
import System.Directory (getCurrentDirectory, withCurrentDirectory)

import qualified Control.Exception as E
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Coverish.Coverage

data LineCoverage
    = Null        -- ^ Not a source line
    | Missed      -- ^ Uncovered source line
    | Covered Int -- ^ Covered some amount of times
    deriving (Eq, Show)

data SourceFile = SourceFile
    { sfPath :: FilePath
    , sfContents :: Text
    , sfCoverage :: [LineCoverage]
    }
    deriving (Eq, Show)

sourceFiles :: [Coverage] -> IO [SourceFile]
sourceFiles = (M.elems <$>) . foldM addCoverage M.empty

addCoverage :: M.Map FilePath SourceFile -> Coverage -> IO (M.Map FilePath SourceFile)
addCoverage m c = do
    esf <- try $ buildSourceFile c

    return $ case esf of
        Left _ -> m -- Leave as-is
        Right sf -> M.insertWith combineSourceFiles (sfPath sf) sf m

buildSourceFile :: Coverage -> IO SourceFile
buildSourceFile c = do
    path <- canonicalizePath $ cPath c
    contents <- T.readFile $ path

    return $ SourceFile
        { sfPath = path
        , sfContents = contents
        , sfCoverage = buildLineCoverage c $ T.lines contents
        }

buildLineCoverage :: Coverage -> [Text] -> [LineCoverage]
buildLineCoverage c = zipWith go [1..]
  where
    go :: Int -> Text -> LineCoverage
    go idx ln
        | idx == cLineNumber c = Covered 1
        | isBlank ln = Null
        | isComment ln = Null
        | otherwise = Missed

    isBlank :: Text -> Bool
    isBlank = T.null . T.dropWhile isSpace

    isComment :: Text -> Bool
    isComment = ("#" `T.isPrefixOf`) . T.dropWhile isSpace

combineSourceFiles :: SourceFile -> SourceFile -> SourceFile
combineSourceFiles sf1 sf2 = sf1
    { sfCoverage = combineLineCoverage (sfCoverage sf1) (sfCoverage sf2) }

combineLineCoverage :: [LineCoverage] -> [LineCoverage] -> [LineCoverage]
combineLineCoverage = zipWith go
  where
    go :: LineCoverage -> LineCoverage -> LineCoverage
    go (Covered x) (Covered y) = Covered $ x + y
    go c@(Covered _) _ = c
    go _ c@(Covered _) = c
    go x _ = x

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fp = do
    let (dir, name) = splitFileName fp
    withCurrentDirectory dir $ (</> name) <$> getCurrentDirectory

try :: IO a -> IO (Either E.IOException a)
try = E.try
