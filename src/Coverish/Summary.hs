{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Coverish.Summary
    ( Summary(..)
    , sTotal
    , sPercent
    , sStrength
    , Summarized(..)
    , SummarizedSourceFile(..)
    , summarizedSourceFile
    ) where

import Prelude

import Coverish.SourceFile
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Ratio ((%))
import Data.Semigroup (Sum(..))
import Data.Semigroup.Generic
import GHC.Generics (Generic)

data Summary = Summary
    { sCovered :: Sum Integer
    , sMissed :: Sum Integer
    , sHits :: Sum Integer
    }
    deriving stock Generic
    deriving (Semigroup, Monoid) via GenericSemigroupMonoid Summary

sTotal :: Summary -> Integer
sTotal s = getSum $ sCovered s <> sMissed s

sPercent :: Summary -> Rational
sPercent s
    | sTotal s == 0 = 0
    | otherwise = getSum (sCovered s) % sTotal s

sStrength :: Summary -> Rational
sStrength s
    | sTotal s == 0 = 0
    | otherwise = getSum (sHits s) % sTotal s

-- brittany-disable-next-binding

instance ToJSON Summary where
    toJSON s = object
        [ "percent" .= sPercent s
        , "strength" .= sStrength s
        , "lines" .= object
           [ "covered" .= getSum (sCovered s)
           , "missed" .= getSum (sMissed s)
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

instance ToJSON SummarizedSourceFile where
    toJSON (SummarizedSourceFile sf s) =
        object ["path" .= sfPath sf, "coverage" .= sfCoverage sf, "totals" .= s]

summarizedSourceFile :: SourceFile -> SummarizedSourceFile
summarizedSourceFile sf =
    SummarizedSourceFile { sSourceFile = sf, sTotals = summarize sf }
