{-# LANGUAGE OverloadedStrings #-}

module Coverish.SourceFile.LineParserSpec
    ( spec
    ) where

import SpecHelper

spec :: Spec
spec = describe "executable" $ do
    it "returns False for blank lines" $ do
        "" `shouldNotSatisfy` executable
        "  " `shouldNotSatisfy` executable
        " \t" `shouldNotSatisfy` executable
        "\t " `shouldNotSatisfy` executable

    it "returns False for commented lines" $ do
        "#" `shouldNotSatisfy` executable
        "# a comment" `shouldNotSatisfy` executable
        "  # with leading space" `shouldNotSatisfy` executable

    it "returns False for function definition beginnings" $ do
        "foo() {" `shouldNotSatisfy` executable
        "function foo() {" `shouldNotSatisfy` executable

        "foo_bar() {" `shouldNotSatisfy` executable
        "function foo_bar() {" `shouldNotSatisfy` executable

        "  bat (  ) { " `shouldNotSatisfy` executable
        "  function bat (  ) { " `shouldNotSatisfy` executable

    it "returns False for function definition endings" $ do
        "}" `shouldNotSatisfy` executable
        "} " `shouldNotSatisfy` executable
        " }" `shouldNotSatisfy` executable

    it "returns False for case patterns" $ do
        " foo*)" `shouldNotSatisfy` executable
        "( foo* )" `shouldNotSatisfy` executable
        "( foo*|*bar )" `shouldNotSatisfy` executable
        "bat) " `shouldNotSatisfy` executable

    it "returns True for other constructions" $ do
        "echo 1" `shouldSatisfy` executable
        "echo 1 # a comment" `shouldSatisfy` executable
        "function() { echo 1; }" `shouldSatisfy` executable
        "}; echo 1" `shouldSatisfy` executable
        "help; }" `shouldSatisfy` executable
