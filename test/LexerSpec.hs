{-# LANGUAGE OverloadedStrings #-}

module LexerSpec (spec) where

import Lexer
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Token

spec :: Spec
spec = do
    describe "numbers" $ do
        describe "integers" $ do
            it "lexes a single digit" $ do
                let input = "1"
                let expected = [TInt 1]
                parseMaybe lexer input `shouldBe` Just expected

            it "lexes a decimal number" $ do
                let input = "123"
                let expected = [TInt 123]
                parseMaybe lexer input `shouldBe` Just expected

            it "lexes a hexadecimal number" $ do
                let input = "0x10"
                let expected = [TInt 16]
                parseMaybe lexer input `shouldBe` Just expected

            it "lexes a binary number" $ do
                let input = "0b11"
                let expected = [TInt 3]
                parseMaybe lexer input `shouldBe` Just expected

        describe "floats" $ do
            it "parses a simple float number" $ do
                let input = "0.1"
                let expected = [TFloat 0.1]
                parseMaybe lexer input `shouldBe` Just expected

            it "parses a small float in scientific notation" $ do
                let input = "1e-2"
                let expected = [TFloat 0.01]
                parseMaybe lexer input `shouldBe` Just expected

            it "parses a big float in scientific notation" $ do
                let input = "1e2"
                let expected = [TFloat 100.0]
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
