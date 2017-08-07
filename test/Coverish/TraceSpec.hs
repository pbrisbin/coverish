{-# LANGUAGE OverloadedStrings #-}
module Coverish.TraceSpec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseTrace" $ do
    it "parses a `set -x` trace log" $ do
        let input = buildTrace
                [ "_coverage:/foo/bar:1:whatever"
                , "_coverage:/baz/bat:2:whatever else"
                , "_coverage:/foo/bar:2:more content\t yeah"
                ]

        input `shouldParseTo` Trace
            [ Execution "/foo/bar" 1
            , Execution "/baz/bat" 2
            , Execution "/foo/bar" 2
            ]

    it "ignores lines without the defined prefix" $ do
        let input = buildTrace
                [ "_coverage:/foo/bar:1:whatever"
                , "Hey there"
                , "_coverage:/foo/bar:2:more content\t yeah"
                , "Again"
                ]

        input `shouldParseTo` Trace
            [ Execution "/foo/bar" 1
            , Execution "/foo/bar" 2
            ]

    it "parses with any number of leading underscore" $ do
        let input = buildTrace
                [ "_coverage:/foo/bar:1:whatever"
                , "__coverage:/foo/bar:2:more content\t yeah"
                ]

        input `shouldParseTo` Trace
            [ Execution "/foo/bar" 1
            , Execution "/foo/bar" 2
            ]

    it "parses with any delimiter" $ do
        let input = buildTrace
                [ "_coverage|/foo/bar|1|whatever"
                , "__coverage|/foo/bar|2|more content\t yeah"
                ]

        input `shouldParseTo` Trace
            [ Execution "/foo/bar" 1
            , Execution "/foo/bar" 2
            ]

    it "still parses with empty paths" $ do
        let input = buildTrace ["_coverage::1:whatever"]

        input `shouldParseTo` Trace [Execution "" 1]

shouldParseTo :: Text -> Trace -> Expectation
x `shouldParseTo` y = parseTrace "test input" x `shouldBe` Right y
