module Parser.Function (pFunctionSignature, pFunctionImplementation) where

import AST.Function
import Parser
import Text.Megaparsec
import Token

pFunctionSignature :: Parser Function
pFunctionSignature = do
  name <- pIdentifier
  _ <- single TColon
  signature <- pType
  _ <- single TSemiColon
  return $ Function name signature []

pFunctionImplementation :: Parser FunctionImplementation
pFunctionImplementation = do
  name <- pIdentifier
  args <- many pIdentifier
  body <- pFunctionBody
  return $ FunctionImplementation name args body

pFunctionBody :: Parser FunctionBody
pFunctionBody = pBlockFunction <|> pSingleLineFunction

pSingleLineFunction :: Parser FunctionBody
pSingleLineFunction = do
  _ <- single TEqual
  expr <- pExpr
  _ <- single TSemiColon
  return $ SingleLineFunction expr

pBlockFunction :: Parser FunctionBody
pBlockFunction = do
  block <- pBlock
  return $ BlockFunction block

pBlock :: Parser Block
pBlock = between (single TLeftBrace) (single TRightBrace) $ do
  statements <- many pStatement
  expr <- pExpr
  return $ Block statements expr

pStatement :: Parser Statement
pStatement = pStatementAssignment <|> pStatementReturn

pStatementAssignment :: Parser Statement
pStatementAssignment = do
  name <- pIdentifier
  _ <- single TEqual
  expr <- pExpr
  _ <- single TSemiColon
  return $ Assignment name expr

pStatementReturn :: Parser Statement
pStatementReturn = do
  _ <- single TReturn
  expr <- pExpr
  return $ Return expr

pExpr :: Parser Expr
pExpr = pIdentifierExpr <|> pLiteralInt <|> pIf

pIdentifierExpr :: Parser Expr
pIdentifierExpr = do
  identifier <- pIdentifier
  return $ Identifier identifier

pLiteralInt :: Parser Expr
pLiteralInt = do
  literal <- satisfy isInt
  case literal of
    TInt i -> return $ LiteralInt i
    _ -> fail "Expected literal int"

pIf :: Parser Expr
pIf = do
  _ <- single TIf
  condition <- pExpr
  trueBranch <- between (single TLeftBrace) (single TRightBrace) pExpr
  _ <- single TElse
  falseBranch <- between (single TLeftBrace) (single TRightBrace) pExpr
  return $ If condition trueBranch falseBranch
