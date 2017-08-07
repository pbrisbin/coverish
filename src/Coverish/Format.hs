{-# LANGUAGE OverloadedStrings #-}
module Coverish.Format
    ( Format(..)
    , format
    ) where

import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Text as T

import Coverish.SourceFile
import Coverish.Summary

newtype Report = Report { rSourceFiles :: [SummarizedSourceFile] }

instance Summarized Report where
    summarize (Report sfs) = mconcat $ map sTotals sfs

instance ToJSON Report where
    toJSON r@(Report sfs) = object
        [ "source_files" .= sfs
        , "totals" .= summarize r
        ]

data Format
    = FJSON
    | FText
    | FRichText

format :: Format -> [SourceFile] -> Text
format FJSON sfs = toStrict $ decodeUtf8 $ encode $ toReport sfs
format FText sfs = T.pack $ unlines
    $ map formatFile ssfs <> ["total: " <> showPercent tsum]
  where
    formatFile ssf = concat
        [ sfPath $ sSourceFile ssf, ": "
        , showPercent $ sTotals ssf
        ]

    ssfs = rSourceFiles report
    tsum = summarize report
    report = toReport sfs

format FRichText _ = error "Rich text format not implemented yet"

showPercent :: Summary -> String
showPercent = show . percent . sPercent
  where
    percent :: Rational -> Double
    percent = (*100) . fromRational

toReport :: [SourceFile] -> Report
toReport = Report . map summarizedSourceFile
