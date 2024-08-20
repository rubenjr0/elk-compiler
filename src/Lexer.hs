{-# LANGUAGE OverloadedStrings #-}

module Lexer (lexer) where

import Token
import Data.Text (Text)
import Data.Void
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

type Lexer = MP.Parsec Void Text

-- Skip spaces and comments
sc :: Lexer ()
sc = L.space MPC.space1 (L.skipLineComment "#") MP.empty

-- Parse a lexeme and skip
lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc

-- Grouping parser
symbolLexer :: Lexer Token
symbolLexer =
  MP.choice
    [ TLeftParen <$ symbol "(",
      TRightParen <$ symbol ")",
      TLeftBracket <$ symbol "{",
      TRightBracket <$ symbol "}",
      TComma <$ symbol ",",
      TSemicolon <$ symbol ";",
      TColon <$ symbol ":",
      TDot <$ symbol ".",
      TArrow <$ symbol "->",
      TPipe <$ symbol "|>",
      TEquals <$ symbol "=",
      TUnderscore <$ symbol "_",
      TQuote <$ symbol "\""
    ]

-- Operator parser
operatorLexer :: Lexer Token
operatorLexer =
  MP.choice
    [ TPlus <$ symbol "+",
      TMinus <$ symbol "-",
      TMultiply <$ symbol "*",
      TDivide <$ symbol "/",
      TExponent <$ symbol "^",
      TPercentage <$ symbol "%",
      TGreaterThan <$ symbol ">",
      TLessThan <$ symbol "<",
      TGreaterThanOrEqual <$ symbol ">=",
      TLessThanOrEqual <$ symbol "<=",
      TAnd <$ symbol "&&",
      TOr <$ symbol "||",
      TNot <$ symbol "!",
      TEquality <$ symbol "==",
      TNotEqual <$ symbol "!="
    ]

-- Keyword parser
keywordLexer :: Lexer Token
keywordLexer =
  MP.choice
    [ TMain <$ symbol "main",
      TType <$ symbol "type",
      TMatch <$ symbol "match"
    ]

-- Identifier parser
identifierLexer :: Lexer Token
identifierLexer = lexeme $ do
  -- underscore char or lowercase letter
  first <- MPC.lowerChar MP.<|> MPC.char '_'
  rest <- MP.many (MPC.alphaNumChar MP.<|> MPC.char '_')
  let ident = first : rest
  return $ TIdentifier ident

upperIdentifierLexer :: Lexer Token
upperIdentifierLexer = lexeme $ do
  first <- MPC.upperChar
  rest <- MP.many MPC.alphaNumChar
  let ident = first : rest
  return $ TUpperIdentifier ident

tokenLexer :: Lexer Token
tokenLexer =
  MP.choice
    [ keywordLexer,
      identifierLexer,
      upperIdentifierLexer,
      symbolLexer,
      operatorLexer
    ]

lexer :: Lexer [Token]
lexer = MP.between sc MP.eof (MP.many tokenLexer)
