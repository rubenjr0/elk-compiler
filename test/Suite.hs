import AST.CustomType
import AST.Type qualified as T
import Parser
import Parser.CustomType
import Test.Hspec
import Text.Megaparsec qualified as MP
import Token

main :: IO ()
main = hspec $ do
  describe "parsing" $ do
    describe "pType" $ do
      it "parses a simple function: U8 -> U8" $ do
        let input = [TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8"]
        let expected = T.Function [T.U8] T.U8
        MP.parseMaybe pType input `shouldBe` Just expected

      it "parses a function: U8 -> U8 -> Bool" $ do
        let input = [TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8", TArrow, TUpperIdentifier "Bool"]
        let expected = T.Function [T.U8, T.U8] T.Bool
        MP.parseMaybe pType input `shouldBe` Just expected

      it "parses a higher order function: (U8 -> U8) -> U8 -> U8" $ do
        let input = [TLeftParen, TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8", TRightParen, TArrow, TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8"]
        let expected = T.Function [T.Function [T.U8] T.U8, T.U8] T.U8
        MP.parseMaybe pType input `shouldBe` Just expected

    describe "pCustomType" $ do
      it "parses a simple custom type with two variants" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBracket, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB", TRightBracket]
        let expected = CustomType "MyType" [Variant "VariantA" [], Variant "VariantB" []]
        MP.parseMaybe pCustomType input `shouldBe` Just expected

      it "fails on missing right bracket" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBracket, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB"]
        MP.parseMaybe pCustomType input `shouldBe` Nothing

      it "parses a custom type with no variants" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBracket, TRightBracket]
        let expected = CustomType "MyType" []
        MP.parseMaybe pCustomType input `shouldBe` Just expected

      it "parses a custom type with a variant with fields" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBracket, TUpperIdentifier "VariantA", TLeftParen, TUpperIdentifier "U8", TRightParen, TRightBracket]
        let expected = CustomType "MyType" [Variant "VariantA" [T.U8]]
        MP.parseMaybe pCustomType input `shouldBe` Just expected

      it "parses a custom type with named fields" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBracket, TIdentifier "field1", TColon, TUpperIdentifier "U8", TRightBracket]
        let expected = CustomType "MyType" [Variant "field1" [T.U8]]
        MP.parseMaybe pCustomType input `shouldBe` Just expected
