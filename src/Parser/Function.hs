module Parser.Function
  ( pFunctionDeclaration,
    pFunctionImplementation,
  )
where

import AST.Function
import Parser
import Text.Megaparsec
import Token

pFunctionDeclaration :: Parser Function
pFunctionDeclaration = do
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
pBlockFunction = BlockFunction <$> pBlock
