module Token
    ( Token (..)
    , isInt
    , isArrow
    , isString
    , isUpperIdentifier
    , isIdentifier
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
    | TLeftBrace
    | TRightBrace
    | TLeftBracket
    | TRightBracket
    | -- Symbols
      TEqual
    | TDot
    | TComma
    | TColon
    | TSemiColon
    | TUnderScore
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
    | TIf
    | TElse
    | TReturn
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

isInt :: Token -> Bool
isInt (TInt _) = True
isInt _ = False

isString :: Token -> Bool
isString (TString _) = True
isString _ = False
