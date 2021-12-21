module Coverish.Trace.LookupSpec
    ( spec
    ) where

import SpecHelper

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

spec :: Spec
spec = do
    d <- runIO getCurrentDirectory
    tl <- runIO $ buildTraceLookup $ Trace
        -- Use some of our own spec files, since we know they'll exist
        [ Execution "test/Coverish/Trace/LookupSpec.hs" 1 1
        , Execution "test/Coverish/TraceSpec.hs" 1 1
        , Execution "/non/existent" 10 1
        , Execution "test/Coverish/Trace/LookupSpec.hs" 2 1
        , Execution "test/Coverish/Trace/LookupSpec.hs" 1 1
        , Execution "test/Coverish/Trace/LookupSpec.hs" 1 1
        , Execution "test/Coverish/Trace/LookupSpec.hs" 5 2 -- multi-line
        ]

    describe "tracePaths" $ do
        it "returns sorted, canonicalized paths from the trace" $ do
            tracePaths tl
                `shouldBe` [ d </> "test/Coverish/Trace/LookupSpec.hs"
                           , d </> "test/Coverish/TraceSpec.hs"
                           ]

    describe "executedInTrace" $ do
        it "returns Just the amount of times a line was executed" $ do
            let absolute = d </> "test/Coverish/Trace/LookupSpec.hs"
            executedInTrace absolute 1 tl `shouldBe` Just 3
            executedInTrace absolute 2 tl `shouldBe` Just 1

            -- multi-line
            executedInTrace absolute 3 tl `shouldBe` Nothing
            executedInTrace absolute 4 tl `shouldBe` Just 1
            executedInTrace absolute 5 tl `shouldBe` Just 1
            executedInTrace absolute 6 tl `shouldBe` Nothing

        it "returns Nothing for unexecuted lines" $ do
            executedInTrace "foo" 1 tl `shouldBe` Nothing

        it "returns Nothing for un-canonicalizable files" $ do
            executedInTrace "/non/existent" 10 tl `shouldBe` Nothing
