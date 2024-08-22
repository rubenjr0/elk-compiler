{-# LANGUAGE OverloadedStrings #-}

module LexerSpec (spec) where

import Lexer
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Token

spec :: Spec
spec = do
    describe "numbers" $ do
        it "lexes a single digit" $ do
            let input = "1"
            let expected = [TInt 1]
            parseMaybe lexer input `shouldBe` Just expected

        it "lexes a multi digit number" $ do
            let input = "123"
            let expected = [TInt 123]
            parseMaybe lexer input `shouldBe` Just expected

    describe "booleans" $ do
        it "lexes True" $ do
            let input = "True"
            let expected = [TBool True]
            parseMaybe lexer input `shouldBe` Just expected

        it "lexes false" $ do
            let input = "False"
            let expected = [TBool False]
            parseMaybe lexer input `shouldBe` Just expected
