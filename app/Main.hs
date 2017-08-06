module Main where

import Coverish
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text.IO as T

main :: IO ()
main = do
    input <- T.getContents

    case parseCoverage "_coverage" "<stdin>" input of
        Right cs -> print cs
        Left err -> do
            hPutStrLn stderr err
            exitFailure
