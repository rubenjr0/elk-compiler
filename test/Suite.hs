import AST.CustomType
import AST.Function
import AST.Type qualified as T
import Parser
import Parser.CustomType
import Parser.Function
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
        let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB", TRightBrace]
        let expected = CustomType "MyType" [Variant "VariantA" [], Variant "VariantB" []]
        MP.parseMaybe pCustomType input `shouldBe` Just expected

      it "fails on missing right bracket" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB"]
        MP.parseMaybe pCustomType input `shouldBe` Nothing

      it "parses a custom type with no variants" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TRightBrace]
        let expected = CustomType "MyType" []
        MP.parseMaybe pCustomType input `shouldBe` Just expected

      it "parses a custom type with a variant with fields" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TLeftParen, TUpperIdentifier "U8", TRightParen, TRightBrace]
        let expected = CustomType "MyType" [Variant "VariantA" [T.U8]]
        MP.parseMaybe pCustomType input `shouldBe` Just expected

      it "parses a custom type with named fields" $ do
        let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TIdentifier "field1", TColon, TUpperIdentifier "U8", TRightBrace]
        let expected = CustomType "MyType" [Variant "field1" [T.U8]]
        MP.parseMaybe pCustomType input `shouldBe` Just expected

    describe "pFunction" $ do
      it "parses a function with no arguments" $ do
        let input = [TIdentifier "my_function", TColon, TUpperIdentifier "U8", TSemiColon]
        let expected = Function "my_function" T.U8 []
        MP.parseMaybe pFunctionSignature input `shouldBe` Just expected

      it "parses a function with arguments" $ do
        let input = [TIdentifier "my_function", TColon, TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8", TSemiColon]
        let expected = Function "my_function" (T.Function [T.U8] T.U8) []
        MP.parseMaybe pFunctionSignature input `shouldBe` Just expected

      it "parses a function's implementation (single line)" $ do
        let input = [TIdentifier "my_function", TIdentifier "arg1", TIdentifier "arg2", TEqual, TInt 42, TSemiColon]
        let expected = FunctionImplementation "my_function" ["arg1", "arg2"] (SingleLineFunction $ LiteralInt 42)
        MP.parseMaybe pFunctionImplementation input `shouldBe` Just expected

      it "parses a function's implementation (simple block)" $ do
        let input = [TIdentifier "my_function", TIdentifier "arg1", TIdentifier "arg2", TLeftBrace, TInt 42, TRightBrace]
        let expected = FunctionImplementation "my_function" ["arg1", "arg2"] (BlockFunction $ Block [] (LiteralInt 42))
        MP.parseMaybe pFunctionImplementation input `shouldBe` Just expected

      it "parses a function's implementation (complex block)" $ do
        let input = [TIdentifier "my_function", TIdentifier "arg1", TIdentifier "arg2", TLeftBrace, TIdentifier "x", TEqual, TInt 42, TSemiColon, TIdentifier "x", TRightBrace]
        let expected = FunctionImplementation "my_function" ["arg1", "arg2"] (BlockFunction $ Block [Assignment "x" (LiteralInt 42)] (Identifier "x"))
        MP.parseMaybe pFunctionImplementation input `shouldBe` Just expected
