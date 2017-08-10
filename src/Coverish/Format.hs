{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Coverish.Format.Color
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

format FRichText sfs = T.unlines $ concatMap formatFile ssfs
  where
    formatFile ssf = showSourceHeader ssf <> showSource (sSourceFile ssf)
    ssfs = rSourceFiles report
    report = toReport sfs

showSourceHeader :: SummarizedSourceFile -> [Text]
showSourceHeader SummarizedSourceFile{..} =
    [ "+-" <> T.replicate hdrWidth "-" <> "-+"
    , "| " <> render header            <> " |"
    , "+-" <> T.replicate hdrWidth "-" <> "-+"
    ]
  where
    header = path <> " (" <> FG Cyan <> perc <> Reset <> ")"
    hdrWidth = visibleLength header

    path = Plain $ T.pack $ sfPath sSourceFile
    perc = Plain $ T.pack $ showPercent sTotals

showSource :: SourceFile -> [Text]
showSource SourceFile{..} = zipWith3 go ([1..] :: [Int]) sfLines sfCoverage
  where
    go i ln cov = render $ idx i <> " " <> FG (escCode cov) <> Plain ln <> Reset

    escCode (Covered _) = Green
    escCode Missed = Red
    escCode Null = LightGray

    idx i = Plain $ T.justifyRight lnWidth ' ' $ T.pack $ show i
    sfLines = T.lines sfContents
    lnWidth = length $ show $ length sfLines

showPercent :: Summary -> String
showPercent = (<> "%") . show . percent . sPercent
  where
    percent :: Rational -> Double
    percent = (*100) . fromRational

toReport :: [SourceFile] -> Report
toReport = Report . map summarizedSourceFile
