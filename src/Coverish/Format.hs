{-# LANGUAGE OverloadedStrings #-}
module Coverish.Format
    ( Format(..)
    , format
    ) where

import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Coverish.SourceFile
import Coverish.Summary

newtype Report = Report [SummarizedSourceFile]

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
format FJSON = toStrict . decodeUtf8 . encode . toReport

format FText = error "Text format not implemented yet"

format FRichText = error "Rich text format not implemented yet"

toReport :: [SourceFile] -> Report
toReport = Report . map summarizedSourceFile
