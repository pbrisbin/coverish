{-# LANGUAGE OverloadedStrings #-}
module Coverish.SourceFile
    ( SourceFile(..)
    , LineCoverage(..)
    , sourceFiles
    ) where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.Text (Text)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Coverish.Trace
import Coverish.Trace.Lookup

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

sourceFiles :: Trace -> IO [SourceFile]
sourceFiles t = do
    tl <- buildTraceLookup t

    forM (tracePaths tl) $ \path -> do
        contents <- either (const "") id <$> safeRead path

        let coverage = zipWith (lookupCoverage tl path) (T.lines contents) [1..]

        return $ SourceFile
            { sfPath = path
            , sfContents = contents
            , sfCoverage = coverage
            }

lookupCoverage :: TraceLookup -> FilePath -> Text -> Int -> LineCoverage
lookupCoverage tl path line lineNo =
    case executedInTrace path lineNo tl of
        Just hits -> Covered hits
        Nothing -> if isBlank line || isComment line then Null else Missed
  where
    isBlank :: Text -> Bool
    isBlank = T.null . T.dropWhile isSpace

    isComment :: Text -> Bool
    isComment = ("#" `T.isPrefixOf`) . T.dropWhile isSpace

safeRead :: FilePath -> IO (Either E.IOException Text)
safeRead = E.try . T.readFile
