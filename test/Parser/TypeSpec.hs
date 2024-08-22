module Parser.TypeSpec (spec) where

import AST.Type qualified as T
import Parser
import Test.Hspec
import Text.Megaparsec
import Token

spec :: Spec
spec = do
    describe "pType" $ do
        it "parses a simple function: U8 -> U8" $ do
            let input = [TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8"]
            let expected = T.Function [T.U8] T.U8
            parseMaybe pType input `shouldBe` Just expected

        it "parses a function: U8 -> U8 -> Bool" $ do
            let input = [TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8", TArrow, TUpperIdentifier "Bool"]
            let expected = T.Function [T.U8, T.U8] T.Bool
            parseMaybe pType input `shouldBe` Just expected

        it "parses a higher order function: (U8 -> U8) -> U8 -> U8" $ do
            let input = [TLeftParen, TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8", TRightParen, TArrow, TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8"]
            let expected = T.Function [T.Function [T.U8] T.U8, T.U8] T.U8
            parseMaybe pType input `shouldBe` Just expected
