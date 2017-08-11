{-# LANGUAGE OverloadedStrings #-}
module Main where

import Coverish

main :: IO ()
main = do
    opts <- parseOptions

    if (oVersion opts)
        then putStrLn versionString
        else do
            trace <- either error id . parseTrace (oInputName opts) <$> oReadInput opts
            sFiles <- sourceFiles $ filterTrace (oFilter opts) trace
            oWriteOutput opts $ format (oFormat opts) sFiles
