{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Serialization.V1 where

import Unison.Prelude

import Prelude hiding (getChar, putChar)

-- import qualified Data.Text as Text
import qualified Unison.Pattern                 as Pattern
import           Unison.Pattern                 ( Pattern
                                                , SeqOp
                                                )
import           Data.Bits                      ( Bits )
import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial              ( serialize
                                                , deserialize
                                                , serializeBE
                                                , deserializeBE
                                                )
import           Data.Bytes.Signed              ( Unsigned )
import           Data.Bytes.VarInt              ( VarInt(..) )
import qualified Data.Map                      as Map
import           Data.List                      ( elemIndex
                                                )
import qualified Unison.Codebase.Branch         as Branch
import qualified Unison.Codebase.Branch.Dependencies as BD
import           Unison.Codebase.Causal         ( Raw(..)
                                                , RawHash(..)
                                                , unRawHash
                                                )
import qualified Unison.Codebase.Causal         as Causal
import qualified Unison.Codebase.Metadata       as Metadata
import           Unison.NameSegment            as NameSegment
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch          as Patch
import           Unison.Codebase.TermEdit       ( TermEdit )
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import           Unison.Hash                    ( Hash )
import           Unison.Kind                    ( Kind )
import           Unison.Reference               ( Reference )
import           Unison.Symbol                  ( Symbol(..) )
import           Unison.Term                    ( Term )
import qualified Data.ByteString               as B
import qualified Data.Sequence                 as Sequence
import qualified Data.Set                      as Set
import qualified Unison.ABT                    as ABT
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Hash                   as Hash
import qualified Unison.Kind                   as Kind
import qualified Unison.Reference              as Reference
import           Unison.Referent               (Referent)
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Util.Star3             ( Star3 )
import qualified Unison.Util.Star3             as Star3
import           Unison.Util.Relation           ( Relation )
import qualified Unison.Util.Relation          as Relation
import qualified Unison.DataDeclaration        as DataDeclaration
import           Unison.DataDeclaration         ( DataDeclaration
                                                , EffectDeclaration
                                                )
import qualified Unison.Var                    as Var
import qualified Unison.ConstructorType        as CT
import Unison.Type (Type)

-- ABOUT THIS FORMAT:
--
-- A serialization format for uncompiled Unison syntax trees.
--
-- Finalized: No
--
-- If Finalized: Yes, don't modify this file in a way that affects serialized form.
-- Instead, create a new file, V(n + 1).
-- This ensures that we have a well-defined serialized form and can read
-- and write old versions.

unknownTag :: (MonadGet m, Show a) => String -> a -> m x
unknownTag msg tag =
  fail $ "unknown tag " ++ show tag ++
         " while deserializing: " ++ msg

getCausal0 :: MonadGet m => m a -> m (Causal.Raw h a)
getCausal0 getA = getWord8 >>= \case
  0 -> RawOne <$> getA
  1 -> flip RawCons <$> (RawHash <$> getHash) <*> getA
  2 -> flip RawMerge . Set.fromList <$> getList (RawHash <$> getHash) <*> getA
  x -> unknownTag "Causal0" x

-- Like getCausal, but doesn't bother to read the actual value in the causal,
-- it just reads the hashes.  Useful for more efficient implementation of
-- `Causal.before`.
-- getCausal00 :: MonadGet m => m Causal00
-- getCausal00 = getWord8 >>= \case
--   0 -> pure One00
--   1 -> Cons00 <$> getHash
--   2 -> Merge00 . Set.fromList <$> getList getHash

-- 1. Can no longer read a causal using just MonadGet;
--    need a way to construct the loader that forms its tail.
--    Same problem with loading Branch0 with monadic tails.
-- 2. Without the monadic tail, need external info to know how to
--    load the tail.  When modifying a nested structure, we
--    need a way to save the intermediate nodes. (e.g. zipper?)
-- 3. We ran into trouble trying to intermingle the marshalling monad
--    (put/get) with the loading/saving monad (io).
-- 4. PutT was weird because we don't think we want the Codebase monad to
--    randomly be able to accumulate bytestrings (put) that don't even reset.
-- 5. We could specialize `Causal m e` to a particular monad that tries to do
--    the right things wrt caching?
-- putCausal0 :: MonadPut m => Causal a -> (a -> m ()) -> m ()
-- putCausal0 = undefined

-- This loads the tail in order to write it?
-- May be crucial to do so, if "loading" tail from `pure`, but
-- otherwise weird.  We'd like to skip writing the tail if it already
-- exists, but how can we tell?
-- Also, we're not even supposed to be writing the tail into the same buffer
-- as head.  We should be writing the hash of the tail though, so we can
-- know which file we need to load it from; loading another file is also
-- something we can't do in this model.
----
-- putCausal :: (MonadPut m, Monad n) => Causal n a -> (a -> m ()) -> n (m ())
-- putCausal (Causal.One hash a) putA =
--   pure $ putWord8 1 *> putHash hash *> putA a
-- putCausal (Causal.ConsN m) putA = do
--   (conss, tail) <- m
--   pure (putWord8 2 *> putFoldable conss (putPair' putHash putA))
--     *> putCausal tail putA
-- putCausal (Causal.Merge hash a tails) putA = do
--   pure (putWord8 3 *> putHash hash *> putA a)
--   putFoldableN (Map.toList tails) $ putPair'' putHash (>>= (`putCausal` putA))
-- putCausal (Causal.Cons _ _ _) _ =
--   error "deserializing 'Causal': the ConsN pattern should have matched here!"


-- getCausal :: MonadGet m => m a -> m (Causal a)
-- getCausal getA = getWord8 >>= \case
--   1 -> Causal.One <$> getHash <*> getA
--   2 -> Causal.consN <$> getList (getPair getHash getA) <*> getCausal getA
--   3 -> Causal.Merge <$> getHash <*> getA <*>
--           (Map.fromList <$> getList (getPair getHash $ getCausal getA))
--   x -> unknownTag "causal" x

-- getCausal ::

getLength ::
  (MonadGet m, Integral n, Integral (Unsigned n),
   Bits n, Bits (Unsigned n))
  => m n
getLength = unVarInt <$> deserialize

getText :: MonadGet m => m Text
getText = do
  len <- getLength
  bs <- B.copy <$> getBytes len
  pure $ decodeUtf8 bs

skipText :: MonadGet m => m ()
skipText = do
  len <- getLength
  void $ getBytes len

getFloat :: MonadGet m => m Double
getFloat = deserializeBE

getNat :: MonadGet m => m Word64
getNat = getWord64be

getInt :: MonadGet m => m Int64
getInt = deserializeBE

getBoolean :: MonadGet m => m Bool
getBoolean = go =<< getWord8 where
  go 0 = pure False
  go 1 = pure True
  go t = unknownTag "Boolean" t

getHash :: MonadGet m => m Hash
getHash = do
  len <- getLength
  bs <- B.copy <$> getBytes len
  pure $ Hash.fromBytes bs

getReference :: MonadGet m => m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Reference.Builtin <$> getText
    1 -> Reference.DerivedId <$> (Reference.Id <$> getHash <*> getLength <*> getLength)
    _ -> unknownTag "Reference" tag

getReferent :: MonadGet m => m Referent
getReferent = do
  tag <- getWord8
  case tag of
    0 -> Referent.Ref <$> getReference
    1 -> Referent.Con <$> getReference <*> getLength <*> getConstructorType
    _ -> unknownTag "getReferent" tag

getConstructorType :: MonadGet m => m CT.ConstructorType
getConstructorType = getWord8 >>= \case
  0 -> pure CT.Data
  1 -> pure CT.Effect
  t -> unknownTag "getConstructorType" t

getMaybe :: MonadGet m => m a -> m (Maybe a)
getMaybe getA = getWord8 >>= \tag -> case tag of
  0 -> pure Nothing
  1 -> Just <$> getA
  _ -> unknownTag "Maybe" tag

getFolded :: MonadGet m => (b -> a -> b) -> b -> m a -> m b
getFolded f z a =
  foldl' f z <$> getList a

getList :: MonadGet m => m a -> m [a]
getList a = getLength >>= (`replicateM` a)

getABT
  :: (MonadGet m, Foldable f, Functor f, Ord v)
  => m v -- getSymbol
  -> m a -- getUnit apparently?
  -> (forall x . m x -> m (f x))
  -> m (ABT.Term f v a)
getABT getVar getA getF = getList getVar >>= go [] where
  go env fvs = do
    a <- getA
    tag <- getWord8
    case tag of
      0 -> do
        tag <- getWord8
        case tag of
          0 -> ABT.annotatedVar a . (env !!) <$> getLength
          1 -> ABT.annotatedVar a . (fvs !!) <$> getLength
          _ -> unknownTag "getABT.Var" tag
      1 -> ABT.tm' a <$> getF (go env fvs)
      2 -> do
        v <- getVar
        body <- go (v:env) fvs
        pure $ ABT.abs' a v body
      3 -> ABT.cycle' a <$> go env fvs
      _ -> unknownTag "getABT" tag

getKind :: MonadGet m => m Kind
getKind = getWord8 >>= \tag -> case tag of
  0 -> pure Kind.Star
  1 -> Kind.Arrow <$> getKind <*> getKind
  _ -> unknownTag "getKind" tag

getType :: (MonadGet m, Ord v)
        => m v -> m a -> m (Type v a)
getType getVar getA = getABT getVar getA go where
  go getChild = getWord8 >>= \tag -> case tag of
    0 -> Type.Ref <$> getReference
    1 -> Type.Arrow <$> getChild <*> getChild
    2 -> Type.Ann <$> getChild <*> getKind
    3 -> Type.App <$> getChild <*> getChild
    4 -> Type.Effect <$> getChild <*> getChild
    5 -> Type.Effects <$> getList getChild
    6 -> Type.Forall <$> getChild
    7 -> Type.IntroOuter <$> getChild
    _ -> unknownTag "getType" tag

getSymbol :: MonadGet m => m Symbol
getSymbol = Symbol <$> getLength <*> (Var.User <$> getText)

getSeqOp :: MonadGet m => m SeqOp
getSeqOp = getWord8 >>= \case
  0   -> pure Pattern.Cons
  1   -> pure Pattern.Snoc
  2   -> pure Pattern.Concat
  tag -> unknownTag "SeqOp" tag

getPattern :: MonadGet m => m a -> m (Pattern a)
getPattern getA = getWord8 >>= \tag -> case tag of
  0 -> Pattern.Unbound <$> getA
  1 -> Pattern.Var <$> getA
  2 -> Pattern.Boolean <$> getA <*> getBoolean
  3 -> Pattern.Int <$> getA <*> getInt
  4 -> Pattern.Nat <$> getA <*> getNat
  5 -> Pattern.Float <$> getA <*> getFloat
  6 -> Pattern.Constructor <$> getA <*> getReference <*> getLength <*> getList
    (getPattern getA)
  7 -> Pattern.As <$> getA <*> getPattern getA
  8 -> Pattern.EffectPure <$> getA <*> getPattern getA
  9 ->
    Pattern.EffectBind
      <$> getA
      <*> getReference
      <*> getLength
      <*> getList (getPattern getA)
      <*> getPattern getA
  10 -> Pattern.SequenceLiteral <$> getA <*> getList (getPattern getA)
  11 ->
    Pattern.SequenceOp
      <$> getA
      <*> getPattern getA
      <*> getSeqOp
      <*> getPattern getA
  12 -> Pattern.Text <$> getA <*> getText
  13 -> Pattern.Char <$> getA <*> getChar
  _ -> unknownTag "Pattern" tag

getTerm :: (MonadGet m, Ord v)
        => m v -> m a -> m (Term v a)
getTerm getVar getA = getABT getVar getA go where
  go getChild = getWord8 >>= \tag -> case tag of
    0 -> Term.Int <$> getInt
    1 -> Term.Nat <$> getNat
    2 -> Term.Float <$> getFloat
    3 -> Term.Boolean <$> getBoolean
    4 -> Term.Text <$> getText
    5 -> Term.Ref <$> getReference
    6 -> Term.Constructor <$> getReference <*> getLength
    7 -> Term.Request <$> getReference <*> getLength
    8 -> Term.Handle <$> getChild <*> getChild
    9 -> Term.App <$> getChild <*> getChild
    10 -> Term.Ann <$> getChild <*> getType getVar getA
    11 -> Term.Sequence . Sequence.fromList <$> getList getChild
    12 -> Term.If <$> getChild <*> getChild <*> getChild
    13 -> Term.And <$> getChild <*> getChild
    14 -> Term.Or <$> getChild <*> getChild
    15 -> Term.Lam <$> getChild
    16 -> Term.LetRec False <$> getList getChild <*> getChild
    17 -> Term.Let False <$> getChild <*> getChild
    18 -> Term.Match <$> getChild
                     <*> getList (Term.MatchCase <$> getPattern getA <*> getMaybe getChild <*> getChild)
    19 -> Term.Char <$> getChar
    20 -> Term.TermLink <$> getReferent
    21 -> Term.TypeLink <$> getReference
    _ -> unknownTag "getTerm" tag

getPair :: MonadGet m => m a -> m b -> m (a,b)
getPair = liftA2 (,)

getTuple3 :: MonadGet m => m a -> m b -> m c -> m (a,b,c)
getTuple3 = liftA3 (,,)

getRelation :: (MonadGet m, Ord a, Ord b) => m a -> m b -> m (Relation a b)
getRelation getA getB = Relation.fromList <$> getList (getPair getA getB)

getMap :: (MonadGet m, Ord a) => m a -> m b -> m (Map a b)
getMap getA getB = Map.fromList <$> getList (getPair getA getB)

getTermEdit :: MonadGet m => m TermEdit
getTermEdit = getWord8 >>= \case
  1 -> TermEdit.Replace <$> getReference <*> (getWord8 >>= \case
    1 -> pure TermEdit.Same
    2 -> pure TermEdit.Subtype
    3 -> pure TermEdit.Different
    t -> unknownTag "TermEdit.Replace" t
    )
  2 -> pure TermEdit.Deprecate
  t -> unknownTag "TermEdit" t

getTypeEdit :: MonadGet m => m TypeEdit
getTypeEdit = getWord8 >>= \case
  1 -> TypeEdit.Replace <$> getReference
  2 -> pure TypeEdit.Deprecate
  t -> unknownTag "TypeEdit" t

getStar3
  :: (MonadGet m, Ord fact, Ord d1, Ord d2, Ord d3)
  => m fact
  -> m d1
  -> m d2
  -> m d3
  -> m (Star3 fact d1 d2 d3)
getStar3 getF getD1 getD2 getD3 =
  Star3.Star3
    <$> (Set.fromList <$> getList getF)
    <*> getRelation getF getD1
    <*> getRelation getF getD2
    <*> getRelation getF getD3

getBranchStar :: (Ord a, Ord n, MonadGet m) => m a -> m n -> m (Branch.Star a n)
getBranchStar getA getN = getStar3 getA getN getMetadataType (getPair getMetadataType getMetadataValue)

getChar :: MonadGet m => m Char
getChar = toEnum . unVarInt <$> deserialize

getNameSegment :: MonadGet m => m NameSegment
getNameSegment = NameSegment <$> getText

getMetadataType :: MonadGet m => m Metadata.Type
getMetadataType = getReference

getMetadataValue :: MonadGet m => m Metadata.Value
getMetadataValue = getReference

getRawBranch :: MonadGet m => m Branch.Raw
getRawBranch =
  Branch.Raw
    <$> getBranchStar getReferent getNameSegment
    <*> getBranchStar getReference getNameSegment
    <*> getMap getNameSegment (RawHash <$> getHash)
    <*> getMap getNameSegment getHash

-- `getBranchDependencies` consumes the same data as `getRawBranch`
getBranchDependencies :: MonadGet m => m (BD.Branches n, BD.Dependencies)
getBranchDependencies = do
  (terms1, types1) <- getTermStarDependencies
  (terms2, types2) <- getTypeStarDependencies
  childHashes <- fmap (RawHash . snd) <$> getList (getPair skipText getHash)
  editHashes <- Set.fromList . fmap snd <$> getList (getPair skipText getHash)
  pure ( childHashes `zip` repeat Nothing
       , BD.Dependencies editHashes (terms1 <> terms2) (types1 <> types2) )
  where
  -- returns things, metadata types, metadata values
  getStarReferences ::
    (MonadGet m, Ord r) => m r -> m ([r], [Metadata.Value])
  getStarReferences getR = do
    void $ getList getR -- throw away the `facts`
    -- d1: references and namesegments
    rs :: [r] <- fmap fst <$> getList (getPair getR skipText)
    -- d2: metadata type index
    void $ getList (getPair getR getMetadataType)
    -- d3: metadata (type, value) index
    (_metadataTypes, metadataValues) <- unzip . fmap snd <$>
      getList (getPair getR (getPair getMetadataType getMetadataValue))
    pure (rs, metadataValues)

  getTermStarDependencies :: MonadGet m => m (Set Reference.Id, Set Reference.Id)
  getTermStarDependencies = do
    (referents, mdValues) <- getStarReferences getReferent
    let termIds = Set.fromList $
          [ i | Referent.Ref (Reference.DerivedId i) <- referents ] ++
          [ i | Reference.DerivedId i <- mdValues ]
        declIds = Set.fromList $
          [ i | Referent.Con (Reference.DerivedId i) _cid _ct <- referents ]
    pure (termIds, declIds)

  getTypeStarDependencies :: MonadGet m => m (Set Reference.Id, Set Reference.Id)
  getTypeStarDependencies = do
    (references, mdValues) <- getStarReferences getReference
    let termIds = Set.fromList $ [ i | Reference.DerivedId i <- mdValues ]
        declIds = Set.fromList $ [ i | Reference.DerivedId i <- references ]
    pure (termIds, declIds)

getDataDeclaration :: (MonadGet m, Ord v) => m v -> m a -> m (DataDeclaration v a)
getDataDeclaration getV getA = DataDeclaration.DataDeclaration <$>
  getModifier <*>
  getA <*>
  getList getV <*>
  getList (getTuple3 getA getV (getType getV getA))

getModifier :: MonadGet m => m DataDeclaration.Modifier
getModifier = getWord8 >>= \case
  0 -> pure DataDeclaration.Structural
  1 -> DataDeclaration.Unique <$> getText
  tag -> unknownTag "DataDeclaration.Modifier" tag

getEffectDeclaration :: (MonadGet m, Ord v) => m v -> m a -> m (EffectDeclaration v a)
getEffectDeclaration getV getA =
  DataDeclaration.EffectDeclaration <$> getDataDeclaration getV getA

getEither :: MonadGet m => m a -> m b -> m (Either a b)
getEither getL getR = getWord8 >>= \case
  0 -> Left <$> getL
  1 -> Right <$> getR
  tag -> unknownTag "Either" tag

formatSymbol :: S.Format Symbol
formatSymbol = S.Format getSymbol putSymbol

getEdits :: MonadGet m => m Patch
getEdits = Patch <$> getRelation getReference getTermEdit
                 <*> getRelation getReference getTypeEdit
