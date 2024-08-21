module Parser
  ( Parser,
    pType,
    pIdentifier,
    pUpperIdentifier,
    pString,
    pBlock,
    pExpr,
    pStatement,
  )
where

import AST.Syntax
import AST.Type qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many, satisfy, sepBy1, single, try, (<|>))
import Token

type Parser = Parsec Void [Token]

pType :: Parser T.Type
pType = try (pFunctionType <|> pNamedType)

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
  token <- satisfy isIdentifier
  case token of
    TIdentifier name -> return name
    _ -> fail "Expected identifier"

pUpperIdentifier :: Parser String
pUpperIdentifier = do
  token <- satisfy isUpperIdentifier
  case token of
    TUpperIdentifier name -> return name
    _ -> fail "Expected upper identifier"

pString :: Parser String
pString = do
  token <- satisfy isString
  case token of
    TString name -> return name
    _ -> fail "Expected string"

pFunctionType :: Parser T.Type
pFunctionType = do
  types <- pFunctionArgs
  case types of
    [t] -> return t
    _ -> return $ T.Function (init types) (last types)

pFunctionArgs :: Parser [T.Type]
pFunctionArgs = sepBy1 (pNamedType <|> pSubFunction) (satisfy isArrow)

pSubFunction :: Parser T.Type
pSubFunction = between (single TLeftParen) (single TRightParen) pFunctionType

pBlock :: Parser Block
pBlock = between (single TLeftBrace) (single TRightBrace) $ do
  statements <- many $ try pStatement
  expr <- pExpr
  return $ Block statements expr

pStatement :: Parser Statement
pStatement = pAssignmentStatement <|> pReturnStatement

pAssignmentStatement :: Parser Statement
pAssignmentStatement = do
  name <- pIdentifier
  _ <- single TEqual
  expr <- pExpr
  _ <- single TSemiColon
  return $ Assignment name expr

pReturnStatement :: Parser Statement
pReturnStatement = do
  _ <- single TReturn
  expr <- pExpr
  return $ Return expr

pExpr :: Parser Expr
pExpr = pIdentifierExpr <|> pLiteralInt <|> pIf

pIdentifierExpr :: Parser Expr
pIdentifierExpr = do
  identifier <- pIdentifier
  return $ IdentifierExpr identifier

pLiteralInt :: Parser Expr
pLiteralInt = do
  literal <- satisfy isInt
  case literal of
    TInt i -> return $ LiteralIntExpr i
    _ -> fail "Expected literal int"

pIf :: Parser Expr
pIf = do
  _ <- single TIf
  condition <- pExpr
  trueBranch <- between (single TLeftBrace) (single TRightBrace) pExpr
  _ <- single TElse
  falseBranch <- between (single TLeftBrace) (single TRightBrace) pExpr
  return $ IfExpr condition trueBranch falseBranch
