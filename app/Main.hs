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

    case parseCoverage "_coverage" "<stdin>" input of
        Right cs -> mapM_ printSourceFile =<< sourceFiles cs
        Left err -> do
            hPutStrLn stderr err
            exitFailure

printSourceFile :: SourceFile -> IO ()
printSourceFile sf = mapM_ T.putStrLn $ zipWith go (sfCoverage sf) $ T.lines $ sfContents sf
  where
    go (Covered _) ln = "G " <> ln
    go Missed      ln = "R " <> ln
    go Null        ln = "  " <> ln
