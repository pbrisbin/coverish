module Coverish.Format
    ( Format(..)
    , format
    ) where

import Prelude

import Coverish.SourceFile
import Coverish.Summary
import Data.Aeson ((.=), ToJSON(..), encode, object)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

newtype Report = Report
    { rSourceFiles :: [SummarizedSourceFile]
    }

instance Summarized Report where
    summarize (Report sfs) = mconcat $ map sTotals sfs

instance ToJSON Report where
    toJSON r@(Report sfs) =
        object ["source_files" .= sfs, "totals" .= summarize r]

data Format
    = FJSON
    | FText
    | FRichText

format :: Format -> [SourceFile] -> Text
format FJSON sfs = toStrict $ decodeUtf8 $ encode $ toReport sfs
format FText sfs =
    pack $ unlines $ map formatFile ssfs <> ["total: " <> showPercent tsum]
  where
    formatFile ssf =
        concat [sfPath $ sSourceFile ssf, ": ", showPercent $ sTotals ssf]

    ssfs = rSourceFiles report
    tsum = summarize report
    report = toReport sfs

format FRichText sfs = T.unlines $ concatMap formatFile ssfs
  where
    formatFile ssf = showSourceHeader ssf <> showSource (sSourceFile ssf)
    ssfs = rSourceFiles report
    report = toReport sfs

showSourceHeader :: SummarizedSourceFile -> [Text]
showSourceHeader SummarizedSourceFile {..} =
    [ "+-" <> T.replicate hdrWidth "-" <> "-+"
    , "| " <> header <> " |"
    , "+-" <> T.replicate hdrWidth "-" <> "-+"
    ]
  where
    header = path <> " (" <> esc "36" <> perc <> reset <> ")"
    hdrWidth = T.length header - 9 -- subtract invisible escapes

    path = pack $ sfPath sSourceFile
    perc = pack $ showPercent sTotals

showSource :: SourceFile -> [Text]
showSource SourceFile {..} = zipWith3 go ([1 ..] :: [Int]) sfLines sfCoverage
  where
    go i ln cov = idx i <> " " <> esc (escCode cov) <> ln <> reset

    escCode (Covered _) = "32"
    escCode Missed = "31"
    escCode Null = "37"

    idx i = T.justifyRight lnWidth ' ' $ pack $ show i
    sfLines = T.lines sfContents
    lnWidth = length $ show $ length sfLines

showPercent :: Summary -> String
showPercent = (<> "%") . show . percent . sPercent
  where
    percent :: Rational -> Double
    percent = (* 100) . fromRational

esc :: Text -> Text
esc c = "\ESC[" <> c <> "m"

reset :: Text
reset = esc "0"

toReport :: [SourceFile] -> Report
toReport = Report . map summarizedSourceFile
