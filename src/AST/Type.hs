module AST.Type
    ( Type (..)
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
    | P8
    | P16
    | P32
    | Bool
    | Char
    | String
    | Unit
    deriving (Show, Eq)
