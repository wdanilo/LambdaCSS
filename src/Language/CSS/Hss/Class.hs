{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.CSS.Hss.Class where

import qualified Prelude as P
import Prologue hiding (assign)
-- import Control.Monad.Free

import           Control.Monad.State.Layered
import           Control.Monad.Free.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Control.Lens                as Lens
import Prelude (round)

import Control.Monad.Trans.Free hiding (wrap)

import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number

instance (PrimMonad m, Functor a) => PrimMonad (FreeT a m) where
  type PrimState (FreeT a m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}






--------------------
-- === Color === ---
--------------------

data Tone
  = RGB Rational Rational Rational




-------------------
-- === Value === --
-------------------

-- === Definition === --

data Val a = Val
  { _valFlags :: Set Text
  , _rawVal   :: RawVal a
  } deriving (Foldable, Functor, Traversable)

data RawVal a
  = Var Text
  | Num Number
  | Txt Text
  | App Text [a]
  deriving (Foldable, Functor, Show, Traversable)

makeLenses  ''Val
deriveShow1 ''Val
deriveShow1 ''RawVal


-- === Standard flag definitions === --

important :: Text
important = "important"


-- === Instances === --

-- Conversions
instance Convertible Number (RawVal a) where convert = Num
instance {-# OVERLAPPABLE #-} Convertible t (RawVal a)
             => Convertible t          (Val a) where convert = convertVia @(RawVal a)
instance t~a => Convertible (RawVal t) (Val a) where convert = Val mempty

-- Pretty
instance Show a => Show (Val a) where
  showsPrec d (Val f r) = showParen' d
    $ showString "Val "
    . showsPrec' (toList f)
    . showString " "
    . showsPrec' r



-------------------
-- === Thunk === --
-------------------

newtype Thunk        = Thunk Int deriving (Num, Show)
type    ThunkMap     = IntMap (Val Thunk)
type    MonadThunk a = MonadState ThunkMap a
makeLenses ''Thunk


-- === Construction === --

newThunk :: MonadThunk m => Val Thunk -> m Thunk
newThunk v = modify @ThunkMap go where
  go m = (wrap idx, IntMap.insert idx v m) where
    idx = IntMap.size m

readThunk    :: MonadThunk m => Thunk -> m (Val Thunk)
readThunkRaw :: MonadThunk m => Thunk -> m (RawVal Thunk)
readThunk    t = (^?! ix (unwrap t)) <$> get @ThunkMap
readThunkRaw t = view rawVal <$> readThunk t

writeThunk :: MonadThunk m => Thunk -> Val Thunk -> m ()
writeThunk t = modifyThunk t . const

modifyThunk :: MonadThunk m => Thunk -> (Val Thunk -> Val Thunk) -> m ()
modifyThunk t f = modify_ @ThunkMap $ ix (unwrap t) %~ f


-- === Catamorphisms === --

fixThunk :: MonadThunk m => Thunk -> m (Fix Val)
fixThunk t = do
  val <- readThunk t
  Fix <$> mapM fixThunk val


-- === Instances === --

instance Convertible Int Thunk where convert = coerce
instance Convertible Thunk Int where convert = coerce



------------------------------
-- === Thunk evaluation === --
------------------------------

-- === Definition === --

newtype ThunkEvaluator m = ThunkEvaluator [Val Thunk -> m (Val Thunk)]
makeLenses ''ThunkEvaluator


-- === Thunk evaluator base class discovery ===

type family GetThunkEvaluationMonad m where
  GetThunkEvaluationMonad (StateT (ThunkEvaluator _) m) = m
  GetThunkEvaluationMonad (t m) = GetThunkEvaluationMonad m

class Monad m => MonadThunkEvaluation m where
  liftThunkEvaluation :: GetThunkEvaluationMonad m a -> m a

instance {-# OVERLAPPABLE #-} (MonadThunkEvaluation m, GetThunkEvaluationMonad (t m) ~ GetThunkEvaluationMonad m, Monad (t m), MonadTrans t)
                 => MonadThunkEvaluation (t m)                         where liftThunkEvaluation = lift . liftThunkEvaluation
instance Monad m => MonadThunkEvaluation (StateT (ThunkEvaluator n) m) where liftThunkEvaluation = lift

type MonadThunkSolver m
  = ( StateData ThunkEvaluator m ~ ThunkEvaluator (GetThunkEvaluationMonad m)
    , MonadState ThunkEvaluator m
    , MonadThunkEvaluation m
    , MonadThunk m
    , Monad (GetThunkEvaluationMonad m))


-- === Construction === --

registerEvaluator :: (StateData ThunkEvaluator m ~ ThunkEvaluator n, MonadState ThunkEvaluator m)
                  => (Val Thunk -> n (Val Thunk)) -> m ()
registerEvaluator f = modify_ @ThunkEvaluator (wrapped %~ (<> [f]))


-- === Utils === --

getSortedThunks :: MonadThunk m => m [Thunk]
getSortedThunks = do
  thunks <- fmap convert . IntMap.keys <$> get @ThunkMap
  reverse . fst <$> foldM (uncurry visit) (mempty, mempty) thunks
  where
    visit :: MonadThunk m => [Thunk] -> Set Int -> Thunk -> m ([Thunk], Set Int)
    visit sorted visited t = if visited ^. contains (unwrap t) then return (sorted, visited) else do
      let visitedMe = visited & contains (unwrap t) .~ True
      (sorted', visited') <- readThunkRaw t >>= \case
        App n ts -> foldM (uncurry visit) (sorted, visitedMe) ts
        _        -> return (sorted, visitedMe)
      return (t : sorted', visited')


-- === Evaluation === --

compileThunkEvaluator :: (MonadThunkEvaluation m, Monad (GetThunkEvaluationMonad m))
                      => ThunkEvaluator (GetThunkEvaluationMonad m) -> (Val Thunk -> m (Val Thunk))
compileThunkEvaluator (ThunkEvaluator fs) val = liftThunkEvaluation $ foldM (flip ($)) val fs

evalThunks :: MonadThunkSolver m => m ()
evalThunks = do
  thunks <- getSortedThunks
  mapM_ evalThunk thunks
  where
    evalThunk :: MonadThunkSolver m => Thunk -> m ()
    evalThunk t = do
      evf <- compileThunkEvaluator <$> get @ThunkEvaluator
      val <- readThunk t
      val' <- evf val
      writeThunk t val'

funcEvaluator :: MonadThunk m => Val Thunk -> m (Val Thunk)
funcEvaluator val = case val ^. rawVal of
  App n ts -> do
    vals <- mapM readThunk ts
    evalApp n vals >>= return . \case
      Just val' -> val' & valFlags .~ (val ^. valFlags)
      Nothing   -> val
  _ -> return val


simplifyUnits :: Number -> Number
simplifyUnits (Number u a) = case Map.lookup "%" u of
  Nothing -> Number u a
  Just i  -> if length (Map.keys u) > 1
    then               Number (mku Nothing)  (a / pow10 i)
    else if i > 1 then Number (mku $ Just 1) (a / pow10 (i - 1))
                  else Number u a
  where mku   a = u & at "%" .~ a
        pow10 i = (100 * 10 ^ (i - 1))

evalApp :: Monad m => Text -> [Val Thunk] -> m (Maybe (Val Thunk))
evalApp n vs = return $ case (n, view rawVal <$> vs) of
  ("*", [Num (Number u a), Num (Number u' a')]) -> Just $ convert $ simplifyUnits $ Number (Map.unionWith (+) u u') (a * a')
  ("/", [Num (Number u a), Num (Number u' a')]) -> Just $ convert $ Number (Map.mergeWithKey (\_ x y -> let w = x + y in justIf (not $ w == 0) w) id id u (negate <$> u')) (a / a')
  ("+", [Num (Number u a), Num (Number u' a')]) -> justIf (u == u') $ convert $ Number u $ a + a'
  _ -> Nothing







simpleVal :: RawVal a -> Val a
simpleVal = Val mempty

val :: MonadThunk m => RawVal Thunk -> m Thunk
val = newThunk . Val mempty

var    :: MonadThunk m => Text   -> m Thunk
number :: MonadThunk m => Number -> m Thunk
txt    :: MonadThunk m => Text   -> m Thunk
app    :: MonadThunk m => Text -> [Thunk] -> m Thunk
var    = val .  Var
number = val .  Num
txt    = val .  Txt
app    = val .: App

appM   :: MonadThunk m => Text -> [m Thunk] -> m Thunk
appM n = app n <=< sequence

appM1 :: MonadThunk m => Text -> m Thunk -> m Thunk
appM2 :: MonadThunk m => Text -> m Thunk -> m Thunk -> m Thunk
appM3 :: MonadThunk m => Text -> m Thunk -> m Thunk -> m Thunk -> m Thunk
appM1 n t1       = appM n [t1]
appM2 n t1 t2    = appM n [t1, t2]
appM3 n t1 t2 t3 = appM n [t1, t2, t3]


-- numApp :: Text -> ([Number] -> Number) -> [Val] -> Val
-- numApp n f args = case sequence (tryToNumber <$> args) of
--   Just args' -> number $ f args'
--   Nothing    -> app n args

-- numApp1 :: Text -> (Number -> Number)                     -> Val -> Val
-- numApp2 :: Text -> (Number -> Number -> Number)           -> Val -> Val -> Val
-- numApp3 :: Text -> (Number -> Number -> Number -> Number) -> Val -> Val -> Val -> Val
-- numApp1 n f t1       = numApp n (\[s1]         -> f s1)       [t1]
-- numApp2 n f t1 t2    = numApp n (\[s1, s2]     -> f s1 s2)    [t1, t2]
-- numApp3 n f t1 t2 t3 = numApp n (\[s1, s2, s3] -> f s1 s2 s3) [t1, t2, t3]

app1 :: MonadThunk m => Text -> Thunk -> m Thunk
app2 :: MonadThunk m => Text -> Thunk -> Thunk -> m Thunk
app3 :: MonadThunk m => Text -> Thunk -> Thunk -> Thunk -> m Thunk
app1 n t1       = app n [t1]
app2 n t1 t2    = app n [t1, t2]
app3 n t1 t2 t3 = app n [t1, t2, t3]


-- data Thunks = Map Text Value





------------------------
-- === Definition === --
------------------------

-- === Definition === --

data Def a = Def
  { _name :: Text
  , _val  :: a
  } deriving (Foldable, Functor, Traversable)

--- === Catamorphisms === --

fixDef :: MonadThunk m => Def Thunk -> m (Def (Fix Val))
fixDef = mapM fixThunk


-- === Instances === --

instance Show a => Show (Def a) where
  showsPrec d (Def n v) = showParen (d > up_prec)
    $ showsPrec (up_prec + 1) n
    . showString " := "
    . showsPrec (up_prec + 1) v
    where up_prec = 5



-------------------
-- === Style === --
-------------------

-- === Definition === --

type MonadStyle = MonadFreeList (Decl Thunk)

type    Style              = StyleT Identity
type    StyleT             = StyleSchemeT Thunk
type    StyleScheme  v     = StyleSchemeT v Identity
newtype StyleSchemeT v m a = StyleSchemeT (FreeListT (Decl v) m a) deriving (Functor, Applicative, Monad, MonadFree (ListCons (Decl v)), MonadTrans)

data Section v = Section
  { _selector :: Selector
  , _body     :: StyleScheme v ()
  }
deriving instance Show v => Show (Section v)

data Decl v
  = DefDecl     (Def     v)
  | SectionDecl (Section v)
deriving instance Show v => Show (Decl v)

data Selector
  = SimpleSelector Text
  | SubSelector    Selector Selector
  deriving (Show)

type ThunkDecl = Decl Thunk
type ValueDecl = Decl (Fix Val)

makeLenses ''StyleSchemeT
makeLenses ''Section


-- === Utils === --

embedDecl        :: Monad m => StyleSchemeT v m (Decl    v) -> StyleSchemeT v m ()
embedSectionDecl :: Monad m => StyleSchemeT v m (Section v) -> StyleSchemeT v m ()
embedDecl d      = d >>= liftToFreeList
embedSectionDecl = embedDecl . fmap SectionDecl

joinStyleT :: Monad m => StyleSchemeT v m a -> m (StyleScheme v a)
joinStyleT = fmap wrap . joinFreeListT . unwrap

fixDecl :: MonadThunk m => ThunkDecl -> m ValueDecl
fixDecl = mapM fixThunk


-- === Instances === --

-- Selectors
instance IsString Selector where
  fromString = SimpleSelector . fromString
instance {-# OVERLAPPABLE #-} (s ~ StyleSchemeT v (StyleSchemeT v m) (), a ~ (), Monad m) => IsString (s -> StyleSchemeT v m a) where
  fromString sel sect = embedSectionDecl (Section (fromString sel) <$> joinStyleT sect)

-- List converions
type instance Item (StyleSchemeT v m a) = Item (Unwrapped (StyleSchemeT v m a))
instance v~v' => Convertible (StyleSchemeT v Identity ()) [Decl v'] where
  convert = convert . unwrap

-- Prim
instance PrimMonad m => PrimMonad (StyleSchemeT v m) where
  type PrimState (StyleSchemeT v m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

-- Pretty
deriving instance Show (Unwrapped (StyleSchemeT v m a)) => Show (StyleSchemeT v m a)

-- Functors
instance Functor     Section where fmap  f = (body . wrapped) %~ mapFreeListT (fmap f)
instance Foldable    Section -- FIXME TODO where foldr f t = foldrFreeList f t . view (body . wrapped)
instance Traversable Section where traverse f = (body . wrapped) $ traverseFreeList (traverse f)

deriving instance Functor     Decl
deriving instance Foldable    Decl
deriving instance Traversable Decl



--------------------------
-- === Expressions === --
--------------------------

-- === Definition === --
-- | The `ExprT` type is defined only to make automatic lifting of literals more type-precise
--   and to awoid obscure inferencer messages.

newtype ExprT      m a = ExprT (IdentityT    m a) deriving (Functor, Applicative, Monad, MonadTrans)
type    StyleExprT v m = ExprT (StyleSchemeT v m)
makeLenses ''ExprT


-- === Running === --

runExprT :: ExprT m a -> m a
runExprT = runIdentityT . unwrap


-- === Assignments === --

class                         (Monad m)                       => AutoAssignment t   m where assignM :: t -> ExprT m Thunk -> m ()
instance {-# OVERLAPPABLE #-} (Monad m, MonadStyle m, t~Text) => AutoAssignment t   m where assignM t v = liftToFreeList =<< runExprT (DefDecl . Def t <$> v)
instance {-# INCOHERENT #-}   (AutoAssignment t m)            => AutoAssignment [t] m where assignM t v = sequence_ $ (flip assignM v) <$> t

assign :: AutoAssignment t m => t -> Thunk -> m ()
assign t v = assignM t (pure v)

infixl 0 =:
(=:) :: AutoAssignment t m => t -> ExprT m Thunk -> m ()
(=:) = assignM


-- === Literals lifting === --

instance MonadThunk m => Num (Unit -> StyleExprT Thunk m Thunk) where
  fromInteger = number .: fromInteger

instance MonadThunk m => Num (StyleExprT Thunk m Thunk) where
  fromInteger = number . Number mempty . fromInteger
  (+)         = appM2 "+"
  (-)         = appM2 "-"
  (*)         = appM2 "*"
  abs         = appM1 "abs"
  signum      = appM1 "signum"

instance MonadThunk m => Fractional (StyleExprT Thunk m Thunk) where
  fromRational = number . Number mempty
  (/)          = appM2 "/"


-- === Instances === --

instance PrimMonad m => PrimMonad (ExprT m) where
  type PrimState (ExprT m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}



----------------------
-- === Renderer === --
----------------------

-- ==== Definition === --

class Renderer style t where
  render :: [ValueDecl] -> Text


-- === Styles === --

data Compact
data Pretty
