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
  | AtomLiteral PSString
  | ObjectLiteral [(PSString, a)]
  | BinaryLiteral [(Integer,Integer)]
  | Tuple2Literal a a
  | TupleLiteral [a]
  deriving (Eq, Ord, Functor)

instance Show a => Show (Literal a) where
  show (BooleanLiteral t) = "Boolean " <> show t
  show (NumericLiteral (Left i)) = "Integer " <> show i
  show (NumericLiteral (Right i)) = "Float " <> show i
  show (CharLiteral c) = "Char " <> show c
  show (StringLiteral s) = "String " <> show s
  show (ListLiteral xs) = "List " <> show xs
  show (AtomLiteral a) = "Atom :" <> show a
  show (ObjectLiteral xs) = "Object " <> show xs
  show (BinaryLiteral xs) = "Binary " <> show xs
  show (TupleLiteral xs) = "(" <> show xs <> ")"
  show (Tuple2Literal a b) = "(" <> show a <> "," <> show b <> ")"
