{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Coverish.SourceFile
    ( SourceFile(..)
    , LineCoverage(..)
    , sourceFiles
    ) where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Coverish.Trace
import Coverish.Trace.Lookup

data LineCoverage
    = Null        -- ^ Not a source line
    | Missed      -- ^ Uncovered source line
    | Covered Int -- ^ Covered some amount of times
    deriving (Eq, Show)

instance A.ToJSON LineCoverage where
    toJSON Null = A.Null
    toJSON Missed = A.Number $ fromIntegral (0 :: Int)
    toJSON (Covered n) = A.Number $ fromIntegral n

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
        -- N.B. tracePaths is expected to only return readable paths, so we're
        -- explicitly not handling exceptions here at this time.
        contents <- T.readFile path

        let coverage = zipWith (lookupCoverage tl path) (T.lines contents) [1..]

        return $ SourceFile
            { sfPath = path
            , sfContents = contents
            , sfCoverage = coverage
            }

lookupCoverage :: TraceLookup -> FilePath -> Text -> Int -> LineCoverage
lookupCoverage tl path line lineNo =
    maybe (unexecuted line) Covered $ executedInTrace path lineNo tl

unexecuted :: Text -> LineCoverage
unexecuted line
    | isBlank line = Null
    | isComment line = Null
    | otherwise = Missed

isBlank :: Text -> Bool
isBlank = T.null . T.dropWhile isSpace

isComment :: Text -> Bool
isComment = ("#" `T.isPrefixOf`) . T.dropWhile isSpace
