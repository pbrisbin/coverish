{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import System.Exit (exitFailure)
import System.FilePath.Glob (Pattern, match)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text.IO as T

import Coverish

data Options = Options
    { oInputName :: String
    , oReadInput :: IO Text
    , oWriteOutput :: Text -> IO ()
    , oFormat :: Format
    , oExclude :: [Pattern]
    , oInclude :: [Pattern]
    }

main :: IO ()
main = do
    let opts = Options
            { oInputName = "<stdin>"
            , oReadInput = T.getContents
            , oWriteOutput = T.putStrLn
            , oFormat = FText -- FJSON
            , oExclude = ["**/*"]
            , oInclude = ["**/downgrade"]
            }

        exclude ex = any (`match` exPath ex) $ oExclude opts
        include ex = any (`match` exPath ex) $ oInclude opts
        tfilter ex = not (exclude ex) || include ex

    etrace <- parseTrace (oInputName opts) <$> oReadInput opts

    case etrace of
        Right t -> do
            sfs <- sourceFiles $ filterTrace tfilter t
            oWriteOutput opts $ format (oFormat opts) sfs

        Left err -> do
            hPutStrLn stderr err
            exitFailure
