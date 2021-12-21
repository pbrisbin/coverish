{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Coverish.SourceFileSpec
    ( spec
    ) where

import SpecHelper

import System.FilePath ((</>))

spec :: Spec
spec = describe "sourceFiles" $ do
    let
        paths =
            [ ("foo/bar", "#!/bin/bash\nif (( 1 )); then\n  echo yes\nfi\n")
            , ("foo/baz", "#!/bin/bash\nif (( 1 )); then\n  echo no\nfi\n")
            ]

    (dir, sfs) <- runIO $ withFileTree paths $ \d -> do
        (d, ) <$> sourceFiles
            (Trace
                [ Execution (d </> "foo/bar") 2 1
                , Execution (d </> "foo/bar") 4 1
                , Execution (d </> "foo/baz") 2 1
                , Execution (d </> "foo/baz") 4 1
                , Execution (d </> "foo/baz") 2 1
                , Execution (d </> "foo/baz") 4 1
                , Execution (d </> "foo/baz") 3 1
                ]
            )

    it "builds a list of SourceFiles from a Trace" $ do
        sfs
            `shouldBe` [ SourceFile
                           { sfPath = dir </> "foo/bar"
                           , sfContents =
                               "#!/bin/bash\nif (( 1 )); then\n  echo yes\nfi\n"
                           , sfCoverage = [Null, Covered 1, Missed, Covered 1]
                           }
                       , SourceFile
                           { sfPath = dir </> "foo/baz"
                           , sfContents =
                               "#!/bin/bash\nif (( 1 )); then\n  echo no\nfi\n"
                           , sfCoverage =
                               [Null, Covered 2, Covered 1, Covered 2]
                           }
                       ]
