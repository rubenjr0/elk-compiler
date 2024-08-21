module AST.Type
  ( Type (..),
    Expr (..),
    Block (..),
    Statement (..),
  )
where

data Type
  = List Type
  | Tuple [Type]
  | Custom String
  | Function [Type] Type
  | -- Built-in types
    I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | F32
  | F64
  | Bool
  | Char
  | String
  | Unit
  deriving (Show, Eq)

data Block = Block [Statement] Expr
  deriving (Show, Eq)

data Statement
  = Assignment String Expr
  | Return Expr
  deriving (Show, Eq)

data Expr
  = IdentifierExpr String
  | LiteralIntExpr Int
  | IfExpr Expr Expr Expr
  deriving (Show, Eq)
