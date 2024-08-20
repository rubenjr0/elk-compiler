module AST.Function where

import AST.Type

-- # signature
-- function_name : Type1 -> Type2 -> ReturnType
--
-- # definition with pattern matching, single line
-- function_name arg1 arg2 = expression;
--
-- # definition with pattern matching, multiple lines
-- function_name arg1 arg2 { ... }
--
-- # arguments can be destructured
-- function_name Some(T) (x:xs) { ... }
