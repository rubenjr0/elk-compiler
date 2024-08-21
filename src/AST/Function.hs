module AST.Function
  ( Function (..),
    FunctionImplementation (..),
    FunctionBody (..),
    Statement (..),
    Block (..),
    Expr (..),
  )
where

import AST.Type

-- # signature, mandatory
-- function_name : Type1 -> Type2 -> ReturnType;
--
-- # definitions with pattern matching, single line or multiple lines,
-- a function can have multiple definitions (for example to cover different matches)
-- function_name arg1 arg2 = expression;
-- function_name arg1 arg2 { ... }
--
-- # arguments can be destructured
-- function_name Some(T) (x:xs) { ... }

data Function = Function
  { function_name :: String,
    function_signature :: Type,
    function_implementations :: [FunctionImplementation]
  }
  deriving (Show, Eq)

data FunctionImplementation = FunctionImplementation
  { function_implementation_name :: String,
    function_arguments :: [String],
    function_body :: FunctionBody
  }
  deriving (Show, Eq)

data FunctionBody
  = SingleLineFunction Expr
  | BlockFunction Block
  deriving (Show, Eq)

data Block = Block [Statement] Expr
  deriving (Show, Eq)

data Statement
  = Assignment String Expr
  | Return Expr
  deriving (Show, Eq)

data Expr
  = Identifier String
  | LiteralInt Int
  | If Expr Expr Expr
  deriving (Show, Eq)
