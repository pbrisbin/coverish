{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Coverish

main :: IO ()
main = do
    input <- T.getContents

    case parseTrace "<stdin>" input of
        Right t -> mapM_ printSourceFile =<< sourceFiles t
        Left err -> do
            hPutStrLn stderr err
            exitFailure

printSourceFile :: SourceFile -> IO ()
printSourceFile sf = do
    putStrLn $ "\ESC[36m" <> sfPath sf <> "\ESC[0m"
    mapM_ T.putStrLn $ zipWith go (sfCoverage sf) $ T.lines $ sfContents sf
  where
    go (Covered _) ln = "\ESC[32m" <> ln <> "\ESC[0m"
    go Missed      ln = "\ESC[31m" <> ln <> "\ESC[0m"
    go Null        ln = ln
