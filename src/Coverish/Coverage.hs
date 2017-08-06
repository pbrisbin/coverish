module Coverish.Coverage
    ( Coverage(..)
    , parseCoverage
    ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.Text

-- | Input definition
data Coverage = Coverage
    { cPath :: FilePath
    -- ^ Parsed exactly as-is, which means it may be empty, relative or
    -- absolute, and the file may or may not exist. Further validation of that
    -- is done at a later stage.
    , cLineNumber :: Int
    }
    deriving (Eq, Show)

-- | Parse the given input to an array of @'Coverage'@
parseCoverage
    :: String -- ^ Prefix for coverage lines to parse (e.g. @_coverage@)
    -> String -- ^ SourceName (file name or @"<stdin>"@)
    -> Text   -- ^ Input stream
    -> Either String [Coverage]
parseCoverage prefix name input = first show $ parse (parser prefix) name input

parser :: String -> Parser [Coverage]
parser prefix = coverageParser prefix `sepEndByIgnoring` restOfLine

-- | @{PREFIX}{DELIM}{PATH}{DELIM}{LINE}{DELIM}
coverageParser :: String -> Parser Coverage
coverageParser prefix = do
    delim <- string prefix *> anyChar

    let delimP = char delim

    Coverage
        <$> manyTill anyChar delimP
        <*> (read <$> manyTill digit delimP)

-- | Apply parser @p@ separated by parser @end@, bug ignore any parse failures
sepEndByIgnoring :: Parser a -> Parser b -> Parser [a]
sepEndByIgnoring p end = catMaybes <$> optionMaybe (try p) `sepEndBy` end

-- | Ignore anything up to, then parse, an @'endOfLine'@
restOfLine :: Parser ()
restOfLine = void $ manyTill anyChar endOfLine
