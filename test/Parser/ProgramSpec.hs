module Parser.ProgramSpec (spec) where

import AST.CustomType
import AST.Function
import AST.Program
import AST.Type qualified as T
import Parser.Program
import Test.Hspec
import Text.Megaparsec
import Token

spec :: Spec
spec = do
    describe "pProgram" $ do
        it "parses a simple program" $ do
            let input = [TType, TUpperIdentifier "MyType", TLeftBrace, TUpperIdentifier "VariantA", TComma, TUpperIdentifier "VariantB", TRightBrace, TIdentifier "my_function", TColon, TUpperIdentifier "MyType", TArrow, TUpperIdentifier "Bool", TSemiColon]
            let expected = Program [] [GlobalCustomTypeDeclaration $ CustomType "MyType" [Variant "VariantA" [], Variant "VariantB" []], GlobalFunctionDeclaration $ Function "my_function" (T.Function [T.Custom "MyType"] T.Bool) []]
            parseMaybe pProgram input `shouldBe` Just expected
