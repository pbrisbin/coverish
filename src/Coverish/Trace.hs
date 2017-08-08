module Coverish.Trace
    ( Trace(..)
    , Execution(..)
    , parseTrace
    , filterTrace
    ) where

import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

data Execution = Execution
    { exPath :: FilePath
    , exLine :: Int
    , exSize :: Int
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

filterTrace :: (Execution -> Bool) -> Trace -> Trace
filterTrace f (Trace es) = Trace $ filter f es

executions :: Parser [Execution]
executions = many execution <* eof

execution :: Parser Execution
execution = do
    delim <- prefixParser *> anyChar

    let delimP = char delim

    path <- manyTill anyChar delimP
    line <- read <$> manyTill digit delimP

    -- ignore rest of line
    void $ manyTill anyChar endOfLine

    -- size is 1 + any number of continuation lines
    size <- (+1) . length . lines <$> manyTill anyToken nextExecution

    return $ Execution path line size

  where
    nextExecution = lookAhead $ try prefixParser <|> eof

prefixParser :: Parser ()
prefixParser = do
    void $ many1 $ char '_'
    void $ string "coverage"
