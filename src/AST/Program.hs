module AST.Program
    ( Program (..)
    , Import (..)
    , GlobalDeclaration (..)
    )
where

import AST.CustomType
import AST.Function (Function, FunctionImplementation)
import AST.Syntax

data Program = Program
    { program_imports :: [Import]
    , program_declarations :: [GlobalDeclaration]
    }
    deriving (Show, Eq)

data Import = Import String
    deriving (Show, Eq)

data GlobalDeclaration
    = GlobalCustomTypeDeclaration CustomType
    | GlobalFunctionDeclaration Function
    | GlobalFunctionImplementation FunctionImplementation
    | MainDeclaration [Statement]
    deriving (Show, Eq)
