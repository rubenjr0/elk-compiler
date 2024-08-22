module Parser.CustomType
    ( pCustomType
    )
where

import AST.CustomType
import Parser
import Text.Megaparsec qualified as MP
import Token

pCustomType :: Parser CustomType
pCustomType = do
    _ <- MP.single TType
    name <- pUpperIdentifier
    _ <- MP.single TLeftBrace
    variants <- pVariant `MP.sepBy` MP.single TComma
    _ <- MP.single TRightBrace
    return $ CustomType name variants

pVariant :: Parser Variant
pVariant = pUnnamedVariant MP.<|> pNamedVariant

pUnnamedVariant :: Parser Variant
pUnnamedVariant = do
    name <- pUpperIdentifier
    -- fields: optional. if present, they are between parenthesis and separated by commas
    -- if not present, it's an empty list
    fields <- MP.between (MP.single TLeftParen) (MP.single TRightParen) (pType `MP.sepBy` MP.single TComma) MP.<|> return []
    return $ Variant name fields

pNamedVariant :: Parser Variant
pNamedVariant = do
    name <- pIdentifier
    _ <- MP.single TColon
    t <- pType
    return $ Variant name [t]
