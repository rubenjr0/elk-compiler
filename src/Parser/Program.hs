module Parser.Program
  ( pProgram,
  )
where

import AST
import Parser
import Parser.CustomType
import Parser.Function
import Text.Megaparsec
import Token

pProgram :: Parser Program
pProgram = do
  imports <- many pImport
  declarations <- many pGlobalDeclaration
  return $ Program imports declarations

pImport :: Parser Import
pImport = do
  _ <- single TImport
  name <- pString
  _ <- single TSemiColon
  return $ Import name

pGlobalDeclaration :: Parser GlobalDeclaration
pGlobalDeclaration =
  pGlobalCustomType
    <|> pGlobalFunctionDeclaration
    <|> pGlobalFunctionImplementation
    <|> pMainDeclaration

pGlobalCustomType :: Parser GlobalDeclaration
pGlobalCustomType = GlobalCustomTypeDeclaration <$> pCustomType

pGlobalFunctionDeclaration :: Parser GlobalDeclaration
pGlobalFunctionDeclaration = GlobalFunctionDeclaration <$> pFunctionDeclaration

pGlobalFunctionImplementation :: Parser GlobalDeclaration
pGlobalFunctionImplementation = GlobalFunctionImplementation <$> pFunctionImplementation

pMainDeclaration :: Parser GlobalDeclaration
pMainDeclaration = do
  _ <- single TMain
  body <- between (single TLeftBrace) (single TRightBrace) $ many pStatement
  return $ MainDeclaration body
