{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempFile)
import System.Process
    (CreateProcess(..), callProcess, createProcess, proc, waitForProcess)

import qualified Data.Map as M

data Options = Options
    { oCommand :: FilePath
    , oOptions :: [String]
    , oCoverishOptions :: [String]
    }

main :: IO ()
main = do
    Options {..} <- parseOptions

    withSystemTempFile "" $ \f _ -> do
        callProcessWithEnv [("COVERISH_TRACE", f)] oCommand oOptions
        callProcess "coverish" $ oCoverishOptions ++ [f]

parseOptions :: IO Options
parseOptions = do
    args <- getArgs

    let
        (before, after) =
            if "--" `elem` args then break (== "--") args else ([], "--" : args)

    case drop 1 after of
        (c : os) -> return Options
            { oCommand = c
            , oOptions = os
            , oCoverishOptions = before
            }

        _ -> error "usage: coverish-exec [[COVERISH OPTIONS] --] CMD [OPTIONS]"

callProcessWithEnv :: [(String, String)] -> FilePath -> [String] -> IO ()
callProcessWithEnv e cmd args = do
    orig <- getEnvironment
    (_, _, _, p) <- createProcess (proc cmd args)
        { delegate_ctlc = True
        , env = Just $ orig `merge` e
        }

    code <- waitForProcess p

    case code of
        ExitSuccess -> return ()
        ExitFailure r -> error $ unlines
            [ "process failed:"
            , "command: " ++ cmd
            , "arguments: " ++ show args
            , "exit code: " ++ show r
            ]

  where
    merge :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
    a `merge` b = M.toList $ M.fromList b `M.union` M.fromList a
