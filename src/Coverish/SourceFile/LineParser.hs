{-# LANGUAGE OverloadedStrings #-}
module Coverish.SourceFile.LineParser
    ( executable
    , parseUnexecutable
    ) where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

-- | Test if a /line/ of shell code is expected to be executed
--
-- When a line does not appear in a @'Trace'@, if it is executable, it should be
-- marked @'Missed'@, otherwise it's @'Null'@ and doesn't impact coverage stats.
--
executable :: Text -> Bool
executable = isLeft . parseUnexecutable

-- | Exported for testing & debugging
parseUnexecutable :: Text -> Either String ()
parseUnexecutable = first show . parse unexecutable ""

unexecutable :: Parser ()
unexecutable = choice
    [ try blank
    , try comment
    , try functionBegin
    , try caseBranch
    , try $ isolated "}"
    , try $ isolated "do"
    , try $ isolated "done"
    , try $ isolated "esac"
    , try $ isolated ";;"
    , try $ isolated ";&"
    , try $ isolated ";;&"
    , try $ isolated "then"
    , try $ isolated "else"
    , try $ isolated "elif"
    , try $ isolated "fi"
    ]

blank :: Parser ()
blank = spaces *> eof

comment :: Parser ()
comment = spaces *> char '#' *> many anyChar *> eof

functionBegin :: Parser ()
functionBegin =
    spaces
        *> optional (try $ string "function")
        *> spaces
        *> many1 (alphaNum <|> char '_')
        *> spaces
        *> char '('
        *> spaces
        *> char ')'
        *> spaces
        *> char '{'
        *> spaces
        *> eof

caseBranch :: Parser ()
caseBranch =
    spaces
        *> optional (char '(')
        *> casePattern
        `sepBy1` char '|'
        *> char ')'
        *> spaces
        *> eof
  where
    casePattern = between spaces spaces $ many1 $ satisfy $ \c ->
        c /= ')' && not (isSpace c)

isolated :: String -> Parser ()
isolated s = spaces *> string s *> spaces *> eof
