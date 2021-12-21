module Main
    ( main
    ) where

import System.Environment (getArgs)
import System.Exit (die)

data Shell = Bash

main :: IO ()
main = do
    shell <- maybe (die usage) pure . parseArgs =<< getArgs
    putStrLn $ evalContent shell

parseArgs :: [String] -> Maybe Shell
parseArgs ("bash" : _) = Just Bash
parseArgs _ = Nothing

usage :: String
usage = unlines ["usage: coverish-eval bash", "  (More shells coming soon.)"]

evalContent :: Shell -> String
evalContent Bash = unlines
    [ "if [ -n \"$COVERISH_TRACE\" ]; then"
    , "  exec 9>>\"$COVERISH_TRACE\""
    , "  export BASH_XTRACEFD=9"
    , "  export PS4='_coverage:$BASH_SOURCE:$LINENO:'"
    , "  set -x"
    , "fi"
    ]
