-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import Prelude.Compat

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.Comments
import Language.PureScript.Types
import Data.Text(Text)

-- | Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder SourceSpan (Literal Binder)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder SourceSpan Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder SourceSpan (Qualified (ProperName 'ConstructorName)) [Binder]
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder SourceSpan (Qualified (OpName 'ValueOpName))
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder Binder Binder Binder
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder SourceSpan Ident Binder
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourceSpan [Comment] Binder
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder SourceType Binder
  | MapBinder [(Binder, Binder)]
  | BinaryBinder [(Binder, Maybe Integer, Maybe [Text])]
  | ListBinder [Binder] Binder
  -- deriving (Show)
instance Show Binder where
  show NullBinder = " _ "
  show (LiteralBinder _ l) = "LiteralB " <> show l
  show (VarBinder _ i) = "VarB " <> show i
  show (ConstructorBinder _ q bs) = "C" ++ show q ++ " " ++ show bs
  show (OpBinder _ q) = "OpB " <> show q
  show (BinaryNoParensBinder b1 b2 b3) = "BinaryNPB " <> show b1 <> show b2 <> show b3
  show (ParensInBinder b) = "ParensInB " <> show b
  show (NamedBinder _ i b) = "NameB " <> show i <> " " <> show b
  show (PositionedBinder _ _ b) = "PositionB " <> show b
  show (TypedBinder _ b) = "TypeB " <> show b
  show (MapBinder xs ) = "MapBindere " <> show xs
  show (BinaryBinder xs) = "BinaryB " <> show xs 
  show (ListBinder bs b) = "ListB " <> show bs <> " " <> show b


-- Manual Eq and Ord instances for `Binder` were added on 2018-03-05. Comparing
-- the `SourceSpan` values embedded in some of the data constructors of `Binder`
-- was expensive. This made exhaustiveness checking observably slow for code
-- such as the `explode` function in `test/purs/passing/LargeSumTypes.purs`.
-- Custom instances were written to skip comparing the `SourceSpan` values. Only
-- the `Ord` instance was needed for the speed-up, but I did not want the `Eq`
-- to have mismatched behavior.
instance Eq Binder where
  (==) NullBinder NullBinder = True
  (==) NullBinder _ = False

  (==) (LiteralBinder _ lb) (LiteralBinder _ lb') = (==) lb lb'
  (==) LiteralBinder{} _ = False

  (==) (VarBinder _ ident) (VarBinder _ ident') = (==) ident ident'
  (==) VarBinder{} _ = False

  (==) (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    (==) qpc qpc' && (==) bs bs'
  (==) ConstructorBinder{} _ = False

  (==) (OpBinder _ qov) (OpBinder _ qov') =
    (==) qov qov'
  (==) OpBinder{} _ = False

  (==) (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    (==) b1 b1' && (==) b2 b2' && (==) b3 b3'
  (==) BinaryNoParensBinder{} _ = False

  (==) (ParensInBinder b) (ParensInBinder b') =
    (==) b b'
  (==) ParensInBinder{} _ = False

  (==) (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    (==) ident ident' && (==) b b'
  (==) NamedBinder{} _ = False

  (==) (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    (==) comments comments' && (==) b b'
  (==) PositionedBinder{} _ = False

  (==) (TypedBinder ty b) (TypedBinder ty' b') =
    (==) ty ty' && (==) b b'
  (==) TypedBinder{} _ = False

  (==) (MapBinder xs) (MapBinder ys) =
    (==) xs ys
  (==) MapBinder{} _ = False

  (==) (BinaryBinder xs) (BinaryBinder ys) =
    (==) xs ys
  (==) BinaryBinder{} _ = False

  (==) (ListBinder xs a) (ListBinder ys b) =
    (==) xs ys && (==) a b
  (==) ListBinder{} _ = False

instance Ord Binder where
  compare NullBinder NullBinder = EQ
  compare NullBinder _ = LT

  compare (LiteralBinder _ lb) (LiteralBinder _ lb') = compare lb lb'
  compare LiteralBinder{} NullBinder = GT
  compare LiteralBinder{} _ = LT

  compare (VarBinder _ ident) (VarBinder _ ident') = compare ident ident'
  compare VarBinder{} NullBinder = GT
  compare VarBinder{} LiteralBinder{} = GT
  compare VarBinder{} _ = LT

  compare (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    compare qpc qpc' <> compare bs bs'
  compare ConstructorBinder{} NullBinder = GT
  compare ConstructorBinder{} LiteralBinder{} = GT
  compare ConstructorBinder{} VarBinder{} = GT
  compare ConstructorBinder{} _ = LT

  compare (OpBinder _ qov) (OpBinder _ qov') =
    compare qov qov'
  compare OpBinder{} NullBinder = GT
  compare OpBinder{} LiteralBinder{} = GT
  compare OpBinder{} VarBinder{} = GT
  compare OpBinder{} ConstructorBinder{} = GT
  compare OpBinder{} _ = LT

  compare (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    compare b1 b1' <> compare b2 b2' <> compare b3 b3'
  compare BinaryNoParensBinder{} ParensInBinder{} = LT
  compare BinaryNoParensBinder{} NamedBinder{} = LT
  compare BinaryNoParensBinder{} PositionedBinder{} = LT
  compare BinaryNoParensBinder{} TypedBinder{} = LT
  compare BinaryNoParensBinder{} _ = GT

  compare (ParensInBinder b) (ParensInBinder b') =
    compare b b'
  compare ParensInBinder{} NamedBinder{} = LT
  compare ParensInBinder{} PositionedBinder{} = LT
  compare ParensInBinder{} TypedBinder{} = LT
  compare ParensInBinder{} _ = GT

  compare (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    compare ident ident' <> compare b b'
  compare NamedBinder{} PositionedBinder{} = LT
  compare NamedBinder{} TypedBinder{} = LT
  compare NamedBinder{} _ = GT

  compare (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    compare comments comments' <> compare b b'
  compare PositionedBinder{} TypedBinder{} = LT
  compare PositionedBinder{} _ = GT

  compare (TypedBinder ty b) (TypedBinder ty' b') =
    compare ty ty' <> compare b b'
  compare TypedBinder{} _ = GT

  compare (MapBinder xs) (MapBinder ys) =
    compare xs ys
  compare MapBinder{} _ = GT

  compare (BinaryBinder xs) (BinaryBinder ys) =
    compare xs ys
  compare BinaryBinder{} _ = GT

  compare (ListBinder xs x) (ListBinder ys y) =
    compare xs ys <> compare x y
  compare ListBinder{} _ = GT

gfst :: (a,b,c) -> a
gfst (a,_,_) = a



binderNames :: Binder -> [Ident]
binderNames = go []
  where
  go ns (LiteralBinder _ b) = lit ns b
  go ns (VarBinder _ name) = name : ns
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (MapBinder xs) = foldl go ns (concat $ fmap (\(a,b) -> [a,b]) xs)
  go ns (ListBinder xs x) = foldl go ns (xs++[x])
  go ns (BinaryBinder xs) = foldl go ns (fmap gfst xs)
  go ns (BinaryNoParensBinder b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder b) = go ns b
  go ns (NamedBinder _ name b) = go (name : ns) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  lit ns (ListLiteral bs) = foldl go ns bs
  lit ns (TupleLiteral xs) = foldl go ns xs 
  lit ns (Tuple2Literal a b) = foldl go ns [a,b] 
  lit ns _ = ns

isIrrefutable :: Binder -> Bool
isIrrefutable NullBinder = True
isIrrefutable (VarBinder _ _) = True
isIrrefutable (PositionedBinder _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ b) = isIrrefutable b
isIrrefutable _ = False
