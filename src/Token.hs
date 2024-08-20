module Token
  ( Token (..),
    isArrow,
    isUpperIdentifier,
    isIdentifier,
  )
where

data Token
  = TIdentifier String
  | TUpperIdentifier String
  | TInt Int
  | TFloat Float
  | TString String
  | -- Groupings
    TLeftParen
  | TRightParen
  | TLeftBracket
  | TRightBracket
  | -- Symbols
    TEquals
  | TDot
  | TComma
  | TColon
  | TSemicolon
  | TUnderscore
  | TArrow
  | TPipe
  | TQuote
  | -- Operators
    TPlus
  | TMinus
  | TMultiply
  | TDivide
  | TExponent
  | TPercentage
  | TGreaterThan
  | TLessThan
  | TGreaterThanOrEqual
  | TLessThanOrEqual
  | TAnd
  | TOr
  | TNot
  | TEquality
  | TNotEqual
  | -- Keywords
    TImport
  | TMain
  | TMatch
  | TType
  | -- Extras
    TComment
  | TNewLine
  | TEOF
  deriving (Show, Eq, Ord)

isUpperIdentifier :: Token -> Bool
isUpperIdentifier (TUpperIdentifier _) = True
isUpperIdentifier _ = False

isIdentifier :: Token -> Bool
isIdentifier (TIdentifier _) = True
isIdentifier _ = False

isArrow :: Token -> Bool
isArrow TArrow = True
isArrow _ = False
