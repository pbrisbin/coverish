module Coverish.Trace
    ( Trace(..)
    , Execution(..)
    , parseTrace
    ) where

import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

data Execution = Execution
    { exPath :: FilePath
    , exLine :: Int
    }
    deriving (Eq, Show)

newtype Trace = Trace [Execution] deriving (Eq, Show)

-- | Parse the output of @set -x@ faithfully
--
-- Requires the trace be formatted in the following way:
--
-- > _coverage${DELIM}${PATH}${DELIM}${LINE}
--
parseTrace :: String -> Text -> Either String Trace
parseTrace name = bimap show Trace . parse executions name

executions :: Parser [Execution]
executions = execution `sepEndByIgnoring` restOfLine

execution :: Parser Execution
execution = do
    delim <- prefixParser *> anyChar

    let delimP = char delim

    Execution
        <$> manyTill anyChar delimP
        <*> (read <$> manyTill digit delimP)

prefixParser :: Parser ()
prefixParser = do
    void $ many1 $ char '_'
    void $ string "coverage"

-- | Apply parser @p@ separated by parser @end@, but ignore any parse failures
sepEndByIgnoring :: Parser a -> Parser b -> Parser [a]
sepEndByIgnoring p end = catMaybes <$> optionMaybe (try p) `sepEndBy` end

-- | Ignore anything up to, then parse, an @'endOfLine'@
restOfLine :: Parser ()
restOfLine = void $ manyTill anyChar endOfLine
