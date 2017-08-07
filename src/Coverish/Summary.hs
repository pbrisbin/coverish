{-# LANGUAGE OverloadedStrings #-}
module Coverish.Summary
    ( Summary
    , Summarized(..)
    , SummarizedSourceFile(..)
    , summarizedSourceFile
    ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Ratio (Rational, (%))

import Coverish.SourceFile

data Summary = Summary
    { sCovered :: Integer
    , sMissed :: Integer
    , sHits :: Integer
    }

sTotal :: Summary -> Integer
sTotal s = sCovered s + sMissed s

sPercent :: Summary -> Rational
sPercent s = sCovered s % sTotal s

sStrength :: Summary -> Rational
sStrength s = sHits s % sTotal s

instance Monoid Summary where
    mempty = Summary 0 0 0
    mappend s1 s2 = Summary
        { sCovered = sCovered s1 + sCovered s2
        , sMissed = sMissed s1 + sMissed s2
        , sHits = sHits s1 + sHits s2
        }

instance ToJSON Summary where
    toJSON s = object
        [ "percent" .= sPercent s
        , "strength" .= sStrength s
        , "lines" .= object
            [ "covered" .= sCovered s
            , "missed" .= sMissed s
            , "total" .= sTotal s
            ]
        ]

class Summarized a where
    summarize :: a -> Summary

instance Summarized LineCoverage where
    summarize Null = mempty
    summarize Missed = mempty { sMissed = 1 }
    summarize (Covered n) = mempty { sCovered = 1, sHits = fromIntegral n }

instance Summarized SourceFile where
    summarize = mconcat . map summarize . sfCoverage

data SummarizedSourceFile = SummarizedSourceFile
    { sSourceFile :: SourceFile
    , sTotals :: Summary
    }

instance ToJSON (SummarizedSourceFile) where
    toJSON (SummarizedSourceFile sf s) = object
        [ "path" .= sfPath sf
        , "coverage" .= sfCoverage sf
        , "totals" .= s
        ]

summarizedSourceFile :: SourceFile -> SummarizedSourceFile
summarizedSourceFile sf = SummarizedSourceFile
    { sSourceFile = sf
    , sTotals = summarize sf
    }
