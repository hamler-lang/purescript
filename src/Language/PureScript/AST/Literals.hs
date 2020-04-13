-- |
-- The core functional representation for literal values.
--
module Language.PureScript.AST.Literals where

import Prelude.Compat
import Language.PureScript.PSString (PSString)

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A boolean literal
  --
  = BooleanLiteral Bool
  -- |
  -- A numeric literal
  --
  | NumericLiteral (Either Integer Double)
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A string literal
  --
  | StringLiteral PSString
  -- |
  -- An list literal
  --
  | ListLiteral [a]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(PSString, a)]
  deriving (Eq, Ord, Show, Functor)
