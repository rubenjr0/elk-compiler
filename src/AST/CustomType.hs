module AST.CustomType
  ( CustomType (..),
    Variant (..),
  )
where

import AST.Type

-- Custop types can have unnamed variants or named variants
-- type CustomType { VariantA, VariantB(TypeA, TypeB) }
-- type CustomType { field: TypeA, field: TypeB }

data CustomType = CustomType
  { custom_type_name :: String,
    custop_type_variants :: [Variant]
  }
  deriving (Show, Eq)

data Variant = Variant
  { variant_name :: String,
    variant_fields :: [Type]
  }
  deriving (Show, Eq)
