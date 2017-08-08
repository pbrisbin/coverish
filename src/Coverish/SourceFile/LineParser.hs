{-# LANGUAGE OverloadedStrings #-}
module Coverish.SourceFile.LineParser
    ( executable
    , parseUnexecutable
    ) where

import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

executable :: Text -> Bool
executable = isLeft . parseUnexecutable

parseUnexecutable :: Text -> Either String ()
parseUnexecutable = first show . parse unexecutable ""

unexecutable :: Parser ()
unexecutable = choice
    [ try $ blank
    , try $ comment
    , try $ functionBegin
    , try $ functionEnd
    ]

blank :: Parser ()
blank = spaces *> eof

comment :: Parser ()
comment = spaces *> char '#' *> many anyChar *> eof

functionBegin :: Parser ()
functionBegin =
    spaces *> optional (try $ string "function") *>
    spaces *> many1 (alphaNum <|> char '_') *>
    spaces *> char '(' *> spaces *> char ')' *>
    spaces *> char '{' *> spaces *> eof

functionEnd :: Parser ()
functionEnd = spaces *> char '}' *> spaces *> eof
