{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text.IO as T

import Coverish

data Options = Options
    { oInputName :: String
    , oReadInput :: IO Text
    , oWriteOutput :: Text -> IO ()
    , oFormat :: Format
    }

main :: IO ()
main = do
    let opts = Options
            { oInputName = "<stdin>"
            , oReadInput = T.getContents
            , oWriteOutput = T.putStrLn
            , oFormat = FText -- FJSON
            }

    etrace <- parseTrace (oInputName opts) <$> oReadInput opts

    case etrace of
        Right t -> do
            sfs <- sourceFiles t
            oWriteOutput opts $ format (oFormat opts) sfs

        Left err -> do
            hPutStrLn stderr err
            exitFailure
