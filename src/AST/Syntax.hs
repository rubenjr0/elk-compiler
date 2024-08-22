module AST.Syntax
    ( Expr (..)
    , Block (..)
    , Statement (..)
    )
where

data Expr
    = IdentifierExpr String
    | LiteralIntExpr Integer
    | IfExpr Expr Expr Expr
    | BlockExpr Block
    deriving (Show, Eq)

data Block = Block [Statement] Expr
    deriving (Show, Eq)

data Statement
    = Assignment String Expr
    | Return Expr
    deriving (Show, Eq)
