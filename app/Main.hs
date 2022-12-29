module Main
    ( main
    ) where

import Prelude

import Coverish
import System.Exit (die)

main :: IO ()
main = do
    opts <- parseOptions

    if oVersion opts
        then putStrLn versionString
        else do
            input <- oReadInput opts
            trace <- either die pure $ parseTrace (oInputName opts) input
            sFiles <- sourceFiles $ filterTrace (oFilter opts) trace
            oWriteOutput opts $ format (oFormat opts) sFiles
