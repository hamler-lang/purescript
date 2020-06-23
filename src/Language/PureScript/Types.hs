{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Data types for types
--
module Language.PureScript.Types where

import Prelude.Compat
import Protolude (ordNub)

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Foldable (fold)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Language.PureScript.AST.SourcePos
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Label (Label)
import Language.PureScript.PSString (PSString)

import Lens.Micro.Platform (Lens', (^.), set)

type SourceType = Type SourceAnn
type SourceConstraint = Constraint SourceAnn

-- |
-- An identifier for the scope of a skolem variable
--
newtype SkolemScope = SkolemScope { runSkolemScope :: Int }
  deriving (Show, Eq, Ord, A.ToJSON, A.FromJSON, Generic)

instance NFData SkolemScope

-- |
-- The type of types
--
data Type a
  -- | A unification variable of type Type
  = TUnknown a Int
  -- | A named type variable
  | TypeVar a Text
  -- | A type-level string
  | TypeLevelString a PSString
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard a (Maybe Text)
  -- | A type constructor
  | TypeConstructor a (Qualified (ProperName 'TypeName))
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp a (Qualified (OpName 'TypeOpName))
  -- | A type application
  | TypeApp a (Type a) (Type a)
  -- | Forall quantifier
  | ForAll a Text (Maybe (Kind a)) (Type a) (Maybe SkolemScope)
  -- | A type with a set of type class constraints
  | ConstrainedType a (Constraint a) (Type a)
  -- | A skolem constant
  | Skolem a Text Int SkolemScope
  -- | An empty row
  | REmpty a
  -- | A non-empty row
  | RCons a Label (Type a) (Type a)
  -- | A row tuple
  | Tuple a (Type a) (Type a)
  -- | A type with a kind annotation
  | KindedType a (Type a) (Kind a)
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParensType a (Type a) (Type a) (Type a)
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  | ParensInType a (Type a)
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Type a)

srcTUnknown :: Int -> SourceType
srcTUnknown = TUnknown NullSourceAnn

srcTypeVar :: Text -> SourceType
srcTypeVar = TypeVar NullSourceAnn

srcTypeLevelString :: PSString -> SourceType
srcTypeLevelString = TypeLevelString NullSourceAnn

srcTypeWildcard :: SourceType
srcTypeWildcard = TypeWildcard NullSourceAnn Nothing

srcTypeConstructor :: Qualified (ProperName 'TypeName) -> SourceType
srcTypeConstructor = TypeConstructor NullSourceAnn

srcTypeOp :: Qualified (OpName 'TypeOpName) -> SourceType
srcTypeOp = TypeOp NullSourceAnn

srcTypeApp :: SourceType -> SourceType -> SourceType
srcTypeApp = TypeApp NullSourceAnn

srcForAll :: Text -> Maybe SourceKind -> SourceType -> Maybe SkolemScope -> SourceType
srcForAll = ForAll NullSourceAnn

srcConstrainedType :: SourceConstraint -> SourceType -> SourceType
srcConstrainedType = ConstrainedType NullSourceAnn

srcSkolem :: Text -> Int -> SkolemScope -> SourceType
srcSkolem = Skolem NullSourceAnn

srcREmpty :: SourceType
srcREmpty = REmpty NullSourceAnn

srcRCons :: Label -> SourceType -> SourceType -> SourceType
srcRCons = RCons NullSourceAnn

srcRTuple :: SourceType -> SourceType -> SourceType
srcRTuple = Tuple NullSourceAnn

srcKindedType :: SourceType -> SourceKind -> SourceType
srcKindedType = KindedType NullSourceAnn

srcBinaryNoParensType :: SourceType -> SourceType -> SourceType -> SourceType
srcBinaryNoParensType = BinaryNoParensType NullSourceAnn

srcParensInType :: SourceType -> SourceType
srcParensInType = ParensInType NullSourceAnn

-- | Additional data relevant to type class constraints
data ConstraintData
  = PartialConstraintData [[Text]] Bool
  -- ^ Data to accompany a Partial constraint generated by the exhaustivity checker.
  -- It contains (rendered) binder information for those binders which were
  -- not matched, and a flag indicating whether the list was truncated or not.
  -- Note: we use 'Text' here because using 'Binder' would introduce a cyclic
  -- dependency in the module graph.
  deriving (Show, Eq, Ord, Generic)

instance NFData ConstraintData

-- | A typeclass constraint
data Constraint a = Constraint
  { constraintAnn :: a
  -- ^ constraint annotation
  , constraintClass :: Qualified (ProperName 'ClassName)
  -- ^ constraint class name
  , constraintArgs  :: [Type a]
  -- ^ type arguments
  , constraintData  :: Maybe ConstraintData
  -- ^ additional data relevant to this constraint
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Constraint a)

srcConstraint :: Qualified (ProperName 'ClassName) -> [SourceType] -> Maybe ConstraintData -> SourceConstraint
srcConstraint = Constraint NullSourceAnn

mapConstraintArgs :: ([Type a] -> [Type a]) -> Constraint a -> Constraint a
mapConstraintArgs f c = c { constraintArgs = f (constraintArgs c) }

overConstraintArgs :: Functor f => ([Type a] -> f [Type a]) -> Constraint a -> f (Constraint a)
overConstraintArgs f c = (\args -> c { constraintArgs = args }) <$> f (constraintArgs c)

constraintDataToJSON :: ConstraintData -> A.Value
constraintDataToJSON (PartialConstraintData bs trunc) =
  A.object
    [ "contents" .= (bs, trunc)
    ]

constraintToJSON :: (a -> A.Value) -> Constraint a -> A.Value
constraintToJSON annToJSON (Constraint {..}) =
  A.object
    [ "constraintAnn"   .= annToJSON constraintAnn
    , "constraintClass" .= constraintClass
    , "constraintArgs"  .= fmap (typeToJSON annToJSON) constraintArgs
    , "constraintData"  .= fmap constraintDataToJSON constraintData
    ]

typeToJSON :: forall a. (a -> A.Value) -> Type a -> A.Value
typeToJSON annToJSON ty =
  case ty of
    TUnknown a b ->
      variant "TUnknown" a b
    TypeVar a b ->
      variant "TypeVar" a b
    TypeLevelString a b ->
      variant "TypeLevelString" a b
    TypeWildcard a b ->
      variant "TypeWildcard" a b
    TypeConstructor a b ->
      variant "TypeConstructor" a b
    TypeOp a b ->
      variant "TypeOp" a b
    TypeApp a b c ->
      variant "TypeApp" a (go b, go c)
    ForAll a b c d e ->
      case c of
        Nothing -> variant "ForAll" a (b, go d, e)
        Just k -> variant "ForAll" a (b, kindToJSON annToJSON k, go d, e)
    ConstrainedType a b c ->
      variant "ConstrainedType" a (constraintToJSON annToJSON b, go c)
    Skolem a b c d ->
      variant "Skolem" a (b, c, d)
    REmpty a ->
      nullary "REmpty" a
    RCons a b c d ->
      variant "RCons" a (b, go c, go d)
    Tuple a c d ->
      variant "RTuple" a (go c, go d)
    KindedType a b c ->
      variant "KindedType" a (go b, kindToJSON annToJSON c)
    BinaryNoParensType a b c d ->
      variant "BinaryNoParensType" a (go b, go c, go d)
    ParensInType a b ->
      variant "ParensInType" a (go b)
  where
  go :: Type a -> A.Value
  go = typeToJSON annToJSON

  variant :: A.ToJSON b => String -> a -> b -> A.Value
  variant tag ann contents =
    A.object
      [ "tag"        .= tag
      , "annotation" .= annToJSON ann
      , "contents"   .= contents
      ]

  nullary :: String -> a -> A.Value
  nullary tag ann =
    A.object
      [ "tag"        .= tag
      , "annotation" .= annToJSON ann
      ]

instance A.ToJSON a => A.ToJSON (Type a) where
  toJSON = typeToJSON A.toJSON

instance A.ToJSON a => A.ToJSON (Constraint a) where
  toJSON = constraintToJSON A.toJSON

instance A.ToJSON ConstraintData where
  toJSON = constraintDataToJSON

constraintDataFromJSON :: A.Value -> A.Parser ConstraintData
constraintDataFromJSON = A.withObject "PartialConstraintData" $ \o -> do
  (bs, trunc) <- o .: "contents"
  pure $ PartialConstraintData bs trunc

constraintFromJSON :: forall a. A.Parser a -> (A.Value -> A.Parser a) -> A.Value -> A.Parser (Constraint a)
constraintFromJSON defaultAnn annFromJSON = A.withObject "Constraint" $ \o -> do
  constraintAnn   <- (o .: "constraintAnn" >>= annFromJSON) <|> defaultAnn
  constraintClass <- o .: "constraintClass"
  constraintArgs  <- o .: "constraintArgs" >>= traverse (typeFromJSON defaultAnn annFromJSON)
  constraintData  <- o .: "constraintData" >>= traverse constraintDataFromJSON
  pure $ Constraint {..}

typeFromJSON :: forall a. A.Parser a -> (A.Value -> A.Parser a) -> A.Value -> A.Parser (Type a)
typeFromJSON defaultAnn annFromJSON = A.withObject "Type" $ \o -> do
  tag <- o .: "tag"
  a   <- (o .: "annotation" >>= annFromJSON) <|> defaultAnn
  let
    contents :: A.FromJSON b => A.Parser b
    contents = o .: "contents"
  case tag of
    "TUnknown" ->
      TUnknown a <$> contents
    "TypeVar" ->
      TypeVar a <$> contents
    "TypeLevelString" ->
      TypeLevelString a <$> contents
    "TypeWildcard" -> do
      b <- contents <|> pure Nothing
      pure $ TypeWildcard a b
    "TypeConstructor" ->
      TypeConstructor a <$> contents
    "TypeOp" ->
      TypeOp a <$> contents
    "TypeApp" -> do
      (b, c) <- contents
      TypeApp a <$> go b <*> go c
    "ForAll" -> do
      let
        withoutMbKind = do
          (b, c, d) <- contents
          ForAll a b Nothing <$> go c <*> pure d
        withMbKind = do
          (b, c, d, e) <- contents
          ForAll a b <$> (Just <$> kindFromJSON defaultAnn annFromJSON c) <*> go d <*> pure e
      withMbKind <|> withoutMbKind
    "ConstrainedType" -> do
      (b, c) <- contents
      ConstrainedType a <$> constraintFromJSON defaultAnn annFromJSON b <*> go c
    "Skolem" -> do
      (b, c, d) <- contents
      pure $ Skolem a b c d
    "REmpty" ->
      pure $ REmpty a
    "RCons" -> do
      (b, c, d) <- contents
      RCons a b <$> go c <*> go d
    "RTuple" -> do
      (c, d) <- contents
      Tuple a <$> go c <*> go d
    "KindedType" -> do
      (b, c) <- contents
      KindedType a <$> go b <*> kindFromJSON defaultAnn annFromJSON c
    "BinaryNoParensType" -> do
      (b, c, d) <- contents
      BinaryNoParensType a <$> go b <*> go c <*> go d
    "ParensInType" -> do
      b <- contents
      ParensInType a <$> go b
    other ->
      fail $ "Unrecognised tag: " ++ other
  where
  go :: A.Value -> A.Parser (Type a)
  go = typeFromJSON defaultAnn annFromJSON

-- These overlapping instances exist to preserve compatibility for common
-- instances which have a sensible default for missing annotations.
instance {-# OVERLAPPING #-} A.FromJSON (Type SourceAnn) where
  parseJSON = typeFromJSON (pure NullSourceAnn) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Type ()) where
  parseJSON = typeFromJSON (pure ()) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON a => A.FromJSON (Type a) where
  parseJSON = typeFromJSON (fail "Invalid annotation") A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Constraint SourceAnn) where
  parseJSON = constraintFromJSON (pure NullSourceAnn) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Constraint ()) where
  parseJSON = constraintFromJSON (pure ()) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON a => A.FromJSON (Constraint a) where
  parseJSON = constraintFromJSON (fail "Invalid annotation") A.parseJSON

instance A.FromJSON ConstraintData where
  parseJSON = constraintDataFromJSON

data RowListItem a = RowListItem
  { rowListAnn :: a
  , rowListLabel :: Label
  , rowListType :: Type a
  } deriving (Show, Generic, Functor, Foldable, Traversable)

srcRowListItem :: Label -> SourceType -> RowListItem SourceAnn
srcRowListItem = RowListItem NullSourceAnn

data TupleItem a = TupleItem
  { tupleAnn :: a
  , tupleType :: Type a
  } deriving (Show, Generic, Functor, Foldable, Traversable)

srcTupleItem :: SourceType -> TupleItem SourceAnn
srcTupleItem = TupleItem NullSourceAnn

tupleToList :: Show a => Type a -> ([TupleItem a])
tupleToList  (Tuple ann ty row) = (TupleItem ann  ty ) : (tupleToList row)
tupleToList  (REmpty _) = [] -- [TupleItem ann (REmpty ann)]
tupleToList x = error $ show x

tupleFromList :: ([TupleItem a], Type a) -> Type a
tupleFromList (xs, r) = foldr (\(TupleItem ann  ty) -> Tuple ann ty) r xs

-- | Convert a row to a list of pairs of labels and types
rowToList :: Type a -> ([RowListItem a], Type a)
rowToList = go where
  go (RCons ann name ty row) =
    first (RowListItem ann name ty :) (rowToList row)
  go r = ([], r)

-- | Convert a row to a list of pairs of labels and types, sorted by the labels.
rowToSortedList :: Type a -> ([RowListItem a], Type a)
rowToSortedList = first (sortBy (comparing rowListLabel)) . rowToList

-- | Convert a list of labels and types to a row
rowFromList :: ([RowListItem a], Type a) -> Type a
rowFromList (xs, r) = foldr (\(RowListItem ann name ty) -> RCons ann name ty) r xs

-- | Check whether a type is a monotype
isMonoType :: Type a -> Bool
isMonoType ForAll{} = False
isMonoType (ParensInType _ t) = isMonoType t
isMonoType (KindedType _ t _) = isMonoType t
isMonoType _        = True

-- | Universally quantify a type
mkForAll :: [(a, (Text, Maybe (Kind a)))] -> Type a -> Type a
mkForAll args ty = foldl (\t (ann, (arg, mbK)) -> ForAll ann arg mbK t Nothing) ty args

-- | Replace a type variable, taking into account variable shadowing
replaceTypeVars :: Text -> Type a -> Type a -> Type a
replaceTypeVars v r = replaceAllTypeVars [(v, r)]

-- | Replace named type variables with types
replaceAllTypeVars :: [(Text, Type a)] -> Type a -> Type a
replaceAllTypeVars = go [] where
  go :: [Text] -> [(Text, Type a)] -> Type a -> Type a
  go _  m (TypeVar ann v) = fromMaybe (TypeVar ann v) (v `lookup` m)
  go bs m (TypeApp ann t1 t2) = TypeApp ann (go bs m t1) (go bs m t2)
  go bs m f@(ForAll ann v mbK t sco)
    | v `elem` keys = go bs (filter ((/= v) . fst) m) f
    | v `elem` usedVars =
      let v' = genName v (keys ++ bs ++ usedVars)
          t' = go bs [(v, TypeVar ann v')] t
      in ForAll ann v' mbK (go (v' : bs) m t') sco
    | otherwise = ForAll ann v mbK (go (v : bs) m t) sco
    where
      keys = map fst m
      usedVars = concatMap (usedTypeVariables . snd) m
  go bs m (ConstrainedType ann c t) = ConstrainedType ann (mapConstraintArgs (map (go bs m)) c) (go bs m t)
  go bs m (RCons ann name' t r) = RCons ann name' (go bs m t) (go bs m r)
  go bs m (Tuple ann t r) = Tuple ann (go bs m t) (go bs m r)
  go bs m (KindedType ann t k) = KindedType ann (go bs m t) k
  go bs m (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go bs m t1) (go bs m t2) (go bs m t3)
  go bs m (ParensInType ann t) = ParensInType ann (go bs m t)
  go _  _ ty = ty

  genName orig inUse = try' 0 where
    try' :: Integer -> Text
    try' n | (orig <> T.pack (show n)) `elem` inUse = try' (n + 1)
           | otherwise = orig <> T.pack (show n)

-- | Collect all type variables appearing in a type
usedTypeVariables :: Type a -> [Text]
usedTypeVariables = ordNub . everythingOnTypes (++) go where
  go (TypeVar _ v) = [v]
  go _ = []

-- | Collect all free type variables appearing in a type
freeTypeVariables :: Type a -> [Text]
freeTypeVariables = ordNub . go [] where
  go :: [Text] -> Type a -> [Text]
  go bound (TypeVar _ v) | v `notElem` bound = [v]
  go bound (TypeApp _ t1 t2) = go bound t1 ++ go bound t2
  go bound (ForAll _ v _ t _) = go (v : bound) t
  go bound (ConstrainedType _ c t) = concatMap (go bound) (constraintArgs c) ++ go bound t
  go bound (RCons _ _ t r) = go bound t ++ go bound r
  go bound (Tuple _  t r) = go bound t ++ go bound r
  go bound (KindedType _ t _) = go bound t
  go bound (BinaryNoParensType _ t1 t2 t3) = go bound t1 ++ go bound t2 ++ go bound t3
  go bound (ParensInType _ t) = go bound t
  go _ _ = []

-- | Universally quantify over all type variables appearing free in a type
quantify :: Type a -> Type a
quantify ty = foldr (\arg t -> ForAll (getAnnForType ty) arg Nothing t Nothing) ty $ freeTypeVariables ty

-- | Move all universal quantifiers to the front of a type
moveQuantifiersToFront :: Type a -> Type a
moveQuantifiersToFront = go [] [] where
  go qs cs (ForAll ann q mbK ty sco) = go ((ann, q, sco, mbK) : qs) cs ty
  go qs cs (ConstrainedType ann c ty) = go qs ((ann, c) : cs) ty
  go qs cs ty = foldl (\ty' (ann, q, sco, mbK) -> ForAll ann q mbK ty' sco) (foldl (\ty' (ann, c) -> ConstrainedType ann c ty') ty cs) qs

-- | Check if a type contains wildcards
containsWildcards :: Type a -> Bool
containsWildcards = everythingOnTypes (||) go where
  go :: Type a -> Bool
  go TypeWildcard{} = True
  go _ = False

-- | Check if a type contains `forall`
containsForAll :: Type a -> Bool
containsForAll = everythingOnTypes (||) go where
  go :: Type a -> Bool
  go ForAll{} = True
  go _ = False

everywhereOnTypes :: (Type a -> Type a) -> Type a -> Type a
everywhereOnTypes f = go where
  go (TypeApp ann t1 t2) = f (TypeApp ann (go t1) (go t2))
  go (ForAll ann arg mbK ty sco) = f (ForAll ann arg mbK (go ty) sco)
  go (ConstrainedType ann c ty) = f (ConstrainedType ann (mapConstraintArgs (map go) c) (go ty))
  go (RCons ann name ty rest) = f (RCons ann name (go ty) (go rest))
  go (Tuple ann ty rest) = f (Tuple ann (go ty) (go rest))
  go (KindedType ann ty k) = f (KindedType ann (go ty) k)
  go (BinaryNoParensType ann t1 t2 t3) = f (BinaryNoParensType ann (go t1) (go t2) (go t3))
  go (ParensInType ann t) = f (ParensInType ann (go t))
  go other = f other

everywhereOnTypesTopDown :: (Type a -> Type a) -> Type a -> Type a
everywhereOnTypesTopDown f = go . f where
  go (TypeApp ann t1 t2) = TypeApp ann (go (f t1)) (go (f t2))
  go (ForAll ann arg mbK ty sco) = ForAll ann arg mbK (go (f ty)) sco
  go (ConstrainedType ann c ty) = ConstrainedType ann (mapConstraintArgs (map (go . f)) c) (go (f ty))
  go (RCons ann name ty rest) = RCons ann name (go (f ty)) (go (f rest))
  go (Tuple ann ty rest) = Tuple ann (go (f ty)) (go (f rest))
  go (KindedType ann ty k) = KindedType ann (go (f ty)) k
  go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go (f t1)) (go (f t2)) (go (f t3))
  go (ParensInType ann t) = ParensInType ann (go (f t))
  go other = f other

everywhereOnTypesM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesM f = go where
  go (TypeApp ann t1 t2) = (TypeApp ann <$> go t1 <*> go t2) >>= f
  go (ForAll ann arg mbK ty sco) = (ForAll ann arg mbK <$> go ty <*> pure sco) >>= f
  go (ConstrainedType ann c ty) = (ConstrainedType ann <$> overConstraintArgs (mapM go) c <*> go ty) >>= f
  go (RCons ann name ty rest) = (RCons ann name <$> go ty <*> go rest) >>= f
  go (Tuple ann ty rest) = (Tuple ann <$> go ty <*> go rest) >>= f
  go (KindedType ann ty k) = (KindedType ann <$> go ty <*> pure k) >>= f
  go (BinaryNoParensType ann t1 t2 t3) = (BinaryNoParensType ann <$> go t1 <*> go t2 <*> go t3) >>= f
  go (ParensInType ann t) = (ParensInType ann <$> go t) >>= f
  go other = f other

everywhereOnTypesTopDownM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesTopDownM f = go <=< f where
  go (TypeApp ann t1 t2) = TypeApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (ForAll ann arg mbK ty sco) = ForAll ann arg mbK <$> (f ty >>= go) <*> pure sco
  go (ConstrainedType ann c ty) = ConstrainedType ann <$> overConstraintArgs (mapM (go <=< f)) c <*> (f ty >>= go)
  go (RCons ann name ty rest) = RCons ann name <$> (f ty >>= go) <*> (f rest >>= go)
  go (Tuple ann ty rest) = Tuple ann <$> (f ty >>= go) <*> (f rest >>= go)
  go (KindedType ann ty k) = KindedType ann <$> (f ty >>= go) <*> pure k
  go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go)
  go (ParensInType ann t) = ParensInType ann <$> (f t >>= go)
  go other = f other

everythingOnTypes :: (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes (<+>) f = go where
  go t@(TypeApp _ t1 t2) = f t <+> go t1 <+> go t2
  go t@(ForAll _ _ _ ty _) = f t <+> go ty
  go t@(ConstrainedType _ c ty) = foldl (<+>) (f t) (map go (constraintArgs c)) <+> go ty
  go t@(RCons _ _ ty rest) = f t <+> go ty <+> go rest
  go t@(Tuple _ ty rest) = f t <+> go ty <+> go rest
  go t@(KindedType _ ty _) = f t <+> go ty
  go t@(BinaryNoParensType _ t1 t2 t3) = f t <+> go t1 <+> go t2 <+> go t3
  go t@(ParensInType _ t1) = f t <+> go t1
  go other = f other

everythingWithContextOnTypes :: s -> r -> (r -> r -> r) -> (s -> Type a -> (s, r)) -> Type a -> r
everythingWithContextOnTypes s0 r0 (<+>) f = go' s0 where
  go' s t = let (s', r) = f s t in r <+> go s' t
  go s (TypeApp _ t1 t2) = go' s t1 <+> go' s t2
  go s (ForAll _ _ _ ty _) = go' s ty
  go s (ConstrainedType _ c ty) = foldl (<+>) r0 (map (go' s) (constraintArgs c)) <+> go' s ty
  go s (RCons _ _ ty rest) = go' s ty <+> go' s rest
  go s (Tuple _ ty rest) = go' s ty <+> go' s rest
  go s (KindedType _ ty _) = go' s ty
  go s (BinaryNoParensType _ t1 t2 t3) = go' s t1 <+> go' s t2 <+> go' s t3
  go s (ParensInType _ t1) = go' s t1
  go _ _ = r0

annForType :: Lens' (Type a) a
annForType k (TUnknown a b) = (\z -> TUnknown z b) <$> k a
annForType k (TypeVar a b) = (\z -> TypeVar z b) <$> k a
annForType k (TypeLevelString a b) = (\z -> TypeLevelString z b) <$> k a
annForType k (TypeWildcard a b) = (\z -> TypeWildcard z b) <$> k a
annForType k (TypeConstructor a b) = (\z -> TypeConstructor z b) <$> k a
annForType k (TypeOp a b) = (\z -> TypeOp z b) <$> k a
annForType k (TypeApp a b c) = (\z -> TypeApp z b c) <$> k a
annForType k (ForAll a b c d e) = (\z -> ForAll z b c d e) <$> k a
annForType k (ConstrainedType a b c) = (\z -> ConstrainedType z b c) <$> k a
annForType k (Skolem a b c d) = (\z -> Skolem z b c d) <$> k a
annForType k (REmpty a) = REmpty <$> k a
annForType k (RCons a b c d) = (\z -> RCons z b c d) <$> k a
annForType k (Tuple a c d) = (\z -> Tuple z c d) <$> k a
annForType k (KindedType a b c) = (\z -> KindedType z b c) <$> k a
annForType k (BinaryNoParensType a b c d) = (\z -> BinaryNoParensType z b c d) <$> k a
annForType k (ParensInType a b) = (\z -> ParensInType z b) <$> k a

getAnnForType :: Type a -> a
getAnnForType = (^. annForType)

setAnnForType :: a -> Type a -> Type a
setAnnForType = set annForType

instance Eq (Type a) where
  (==) = eqType

instance Ord (Type a) where
  compare = compareType

eqType :: Type a -> Type b -> Bool
eqType (TUnknown _ a) (TUnknown _ a') = a == a'
eqType (TypeVar _ a) (TypeVar _ a') = a == a'
eqType (TypeLevelString _ a) (TypeLevelString _ a') = a == a'
eqType (TypeWildcard _ a) (TypeWildcard _ a') = a == a'
eqType (TypeConstructor _ a) (TypeConstructor _ a') = a == a'
eqType (TypeOp _ a) (TypeOp _ a') = a == a'
eqType (TypeApp _ a b) (TypeApp _ a' b') = eqType a a' && eqType b b'
eqType (ForAll _ a b c d) (ForAll _ a' b' c' d') = a == a' && eqMaybeKind b b' && eqType c c' && d == d'
eqType (ConstrainedType _ a b) (ConstrainedType _ a' b') = eqConstraint a a' && eqType b b'
eqType (Skolem _ a b c) (Skolem _ a' b' c') = a == a' && b == b' && c == c'
eqType (REmpty _) (REmpty _) = True
eqType (RCons _ a b c) (RCons _ a' b' c') = a == a' && eqType b b' && eqType c c'
eqType (Tuple _ b c) (Tuple _  b' c') = eqType b b' && eqType c c'
eqType (KindedType _ a b) (KindedType _ a' b') = eqType a a' && eqKind b b'
eqType (BinaryNoParensType _ a b c) (BinaryNoParensType _ a' b' c') = eqType a a' && eqType b b' && eqType c c'
eqType (ParensInType _ a) (ParensInType _ a') = eqType a a'
eqType _ _ = False

compareType :: Type a -> Type b -> Ordering
compareType (TUnknown _ a) (TUnknown _ a') = compare a a'
compareType (TUnknown {}) _ = LT

compareType (TypeVar _ a) (TypeVar _ a') = compare a a'
compareType (TypeVar {}) _ = LT
compareType _ (TypeVar {}) = GT

compareType (TypeLevelString _ a) (TypeLevelString _ a') = compare a a'
compareType (TypeLevelString {}) _ = LT
compareType _ (TypeLevelString {}) = GT

compareType (TypeWildcard _ a) (TypeWildcard _ a') = compare a a'
compareType (TypeWildcard {}) _ = LT
compareType _ (TypeWildcard {}) = GT

compareType (TypeConstructor _ a) (TypeConstructor _ a') = compare a a'
compareType (TypeConstructor {}) _ = LT
compareType _ (TypeConstructor {}) = GT

compareType (TypeOp _ a) (TypeOp _ a') = compare a a'
compareType (TypeOp {}) _ = LT
compareType _ (TypeOp {}) = GT

compareType (TypeApp _ a b) (TypeApp _ a' b') = compareType a a' <> compareType b b'
compareType (TypeApp {}) _ = LT
compareType _ (TypeApp {}) = GT

compareType (ForAll _ a b c d) (ForAll _ a' b' c' d') = compare a a' <> compareMaybeKind b b' <> compareType c c' <> compare d d'
compareType (ForAll {}) _ = LT
compareType _ (ForAll {}) = GT

compareType (ConstrainedType _ a b) (ConstrainedType _ a' b') = compareConstraint a a' <> compareType b b'
compareType (ConstrainedType {}) _ = LT
compareType _ (ConstrainedType {}) = GT

compareType (Skolem _ a b c) (Skolem _ a' b' c') = compare a a' <> compare b b' <> compare c c'
compareType (Skolem {}) _ = LT
compareType _ (Skolem {}) = GT

compareType (REmpty _) (REmpty _) = EQ
compareType (REmpty _) _ = LT
compareType _ (REmpty _) = GT

compareType (RCons _ a b c) (RCons _ a' b' c') = compare a a' <> compareType b b' <> compareType c c'
compareType (RCons {}) _ = LT
compareType _ (RCons {}) = GT

compareType (Tuple _ b c) (Tuple _ b' c') = compareType b b' <> compareType c c'
compareType (Tuple {}) _ = LT
compareType _ (Tuple {}) = GT

compareType (KindedType _ a b) (KindedType _ a' b') = compareType a a' <> compareKind b b'
compareType (KindedType {}) _ = LT
compareType _ (KindedType {}) = GT

compareType (BinaryNoParensType _ a b c) (BinaryNoParensType _ a' b' c') = compareType a a' <> compareType b b' <> compareType c c'
compareType (BinaryNoParensType {}) _ = LT
compareType _ (BinaryNoParensType {}) = GT

compareType (ParensInType _ a) (ParensInType _ a') = compareType a a'
compareType (ParensInType {}) _ = GT

instance Eq (Constraint a) where
  (==) = eqConstraint

instance Ord (Constraint a) where
  compare = compareConstraint

eqConstraint :: Constraint a -> Constraint b -> Bool
eqConstraint (Constraint _ a b c) (Constraint _ a' b' c') = a == a' && and (zipWith eqType b b') && c == c'

compareConstraint :: Constraint a -> Constraint b -> Ordering
compareConstraint (Constraint _ a b c) (Constraint _ a' b' c') = compare a a' <> fold (zipWith compareType b b') <> compare c c'
