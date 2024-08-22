module AST.Function
    ( Function (..)
    , FunctionImplementation (..)
    , FunctionBody (..)
    , Statement (..)
    , Block (..)
    , Expr (..)
    )
where

import AST.Syntax
import AST.Type

data Function = Function
    { function_name :: String
    , function_signature :: Type
    , function_implementations :: [FunctionImplementation]
    }
    deriving (Show, Eq)

data FunctionImplementation = FunctionImplementation
    { function_implementation_name :: String
    , function_arguments :: [String]
    , function_body :: FunctionBody
    }
    deriving (Show, Eq)

data FunctionBody
    = SingleLineFunction Expr
    | BlockFunction Block
    deriving (Show, Eq)
