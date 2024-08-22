module Parser.FunctionSpec (spec) where

import AST.Function
import AST.Type qualified as T
import Parser.Function
import Test.Hspec
import Text.Megaparsec
import Token

spec :: Spec
spec = do
    describe "pFunction" $ do
        it "parses a function with no arguments" $ do
            let input = [TIdentifier "my_function", TColon, TUpperIdentifier "U8", TSemiColon]
            let expected = Function "my_function" T.U8 []
            parseMaybe pFunctionDeclaration input `shouldBe` Just expected

        it "parses a function with arguments" $ do
            let input = [TIdentifier "my_function", TColon, TUpperIdentifier "U8", TArrow, TUpperIdentifier "U8", TSemiColon]
            let expected = Function "my_function" (T.Function [T.U8] T.U8) []
            parseMaybe pFunctionDeclaration input `shouldBe` Just expected

        it "parses a function with a custom type argument" $ do
            let input = [TIdentifier "my_function", TColon, TUpperIdentifier "MyType", TArrow, TUpperIdentifier "Bool", TSemiColon]
            let expected = Function "my_function" (T.Function [T.Custom "MyType"] T.Bool) []
            parseMaybe pFunctionDeclaration input `shouldBe` Just expected

        it "parses a function's implementation (single line)" $ do
            let input = [TIdentifier "my_function", TIdentifier "arg1", TIdentifier "arg2", TEqual, TInt 42, TSemiColon]
            let expected = FunctionImplementation "my_function" ["arg1", "arg2"] (SingleLineFunction $ LiteralIntExpr 42)
            parseMaybe pFunctionImplementation input `shouldBe` Just expected

        it "parses a function's implementation (simple block)" $ do
            let input = [TIdentifier "my_function", TIdentifier "arg1", TIdentifier "arg2", TLeftBrace, TInt 42, TRightBrace]
            let expected = FunctionImplementation "my_function" ["arg1", "arg2"] (BlockFunction $ Block [] (LiteralIntExpr 42))
            parseMaybe pFunctionImplementation input `shouldBe` Just expected

        it "parses a function's implementation (complex block)" $ do
            let input = [TIdentifier "my_function", TIdentifier "arg1", TIdentifier "arg2", TLeftBrace, TIdentifier "x", TEqual, TInt 42, TSemiColon, TIdentifier "x", TRightBrace]
            let expected = FunctionImplementation "my_function" ["arg1", "arg2"] (BlockFunction $ Block [Assignment "x" (LiteralIntExpr 42)] (IdentifierExpr "x"))
            parseMaybe pFunctionImplementation input `shouldBe` Just expected
