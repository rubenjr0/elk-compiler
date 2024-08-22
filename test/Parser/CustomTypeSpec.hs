module Parser.CustomTypeSpec (spec) where

import AST.CustomType
import AST.Type qualified as T
import Parser.CustomType
import Test.Hspec
import Text.Megaparsec
import Token

spec :: Spec
spec = do
    describe "pCustomType" $ do
        it "parses a simple custom type with two variants" $ do
            let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB", TRightBrace]
            let expected = CustomType "MyType" [Variant "VariantA" [], Variant "VariantB" []]
            parseMaybe pCustomType input `shouldBe` Just expected

        it "fails on missing right bracket" $ do
            let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB"]
            parseMaybe pCustomType input `shouldBe` Nothing

        it "parses a custom type with no variants" $ do
            let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TRightBrace]
            let expected = CustomType "MyType" []
            parseMaybe pCustomType input `shouldBe` Just expected

        it "parses a custom type with a variant with fields" $ do
            let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TLeftParen, TUpperIdentifier "U8", TRightParen, TRightBrace]
            let expected = CustomType "MyType" [Variant "VariantA" [T.U8]]
            parseMaybe pCustomType input `shouldBe` Just expected

        it "parses a custom type with named fields" $ do
            let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TIdentifier "field1", TColon, TUpperIdentifier "U8", TRightBrace]
            let expected = CustomType "MyType" [Variant "field1" [T.U8]]
            parseMaybe pCustomType input `shouldBe` Just expected
