module Parser (Parser, pType, pIdentifier, pUpperIdentifier) where

import AST.Type qualified as T
import Data.Void (Void)
import Text.Megaparsec qualified as MP
import Token

type Parser = MP.Parsec Void [Token]

pType :: Parser T.Type
pType = MP.try (pFunctionType MP.<|> pNamedType)

pNamedType :: Parser T.Type
pNamedType = do
  name <- pUpperIdentifier
  case name of
    "I8" -> return T.I8
    "I16" -> return T.I16
    "I32" -> return T.I32
    "I64" -> return T.I64
    "U8" -> return T.U8
    "U16" -> return T.U16
    "U32" -> return T.U32
    "U64" -> return T.U64
    "F32" -> return T.F32
    "F64" -> return T.F64
    "Bool" -> return T.Bool
    "Char" -> return T.Char
    "String" -> return T.String
    "Unit" -> return T.Unit
    _ -> return $ T.Custom name

pIdentifier :: Parser String
pIdentifier = do
  token <- MP.satisfy isIdentifier
  case token of
    TIdentifier name -> return name
    _ -> fail "Expected identifier"

pUpperIdentifier :: Parser String
pUpperIdentifier = do
  token <- MP.satisfy isUpperIdentifier
  case token of
    TUpperIdentifier name -> return name
    _ -> fail "Expected upper identifier"

pFunctionType :: Parser T.Type
pFunctionType = do
  types <- pFunctionArgs
  case types of
    [t] -> return t
    _ -> return $ T.Function (init types) (last types)

pFunctionArgs :: Parser [T.Type]
pFunctionArgs = MP.sepBy1 (pNamedType MP.<|> pSubFunction) (MP.satisfy isArrow)

pSubFunction :: Parser T.Type
pSubFunction = MP.between (MP.single TLeftParen) (MP.single TRightParen) pFunctionType
