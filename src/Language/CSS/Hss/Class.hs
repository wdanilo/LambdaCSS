{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.CSS.Hss.Class where

import qualified Prelude as P
import Prologue hiding (assign)
-- import Control.Monad.Free

import           Control.Monad.State.Layered
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Control.Lens       as Lens
import Prelude (round)

import Data.Functor.Foldable
import Data.Bifunctor.TH
import Text.Show.Deriving

import Control.Monad.Trans.Free hiding (wrap)

instance (PrimMonad m, Functor a) => PrimMonad (FreeT a m) where
  type PrimState (FreeT a m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}



----------------------
-- === FreeList === --
----------------------

-- === Definition === --

type    MonadFreeList t     = MonadFree (ListCons t)
type    FreeList      t     = FreeListT t Identity
newtype FreeListT     t m a = FreeListT (FreeT (ListCons t) m a)
                              deriving (Applicative, Functor, Monad, MonadFreeList t, MonadTrans)

data ListCons t next = ListCons t next
  deriving (Show, Functor, Foldable, Traversable)

makeLenses          ''FreeListT
deriveBifunctor     ''ListCons
deriveBifoldable    ''ListCons
deriveBitraversable ''ListCons


-- === Utils === --

liftToFreeList :: MonadFreeList t m => t -> m ()
liftToFreeList a = liftF $ ListCons a ()

joinFreeListT :: Monad m => FreeListT t m a -> m (FreeList t a)
joinFreeListT = fmap wrap . joinFreeT . unwrap

mapFreeListT :: Monad m => (t -> t') -> FreeListT t m a -> FreeListT t' m a
mapFreeListT f (FreeListT ft) = FreeListT $ go ft where
  go (FreeT ml) = FreeT $ do
    ml >>= \case
      Pure a   -> return $ Pure a
      Free lst -> return $ Free $ bimap f go lst

foldrFreeList :: forall t t' a. (t -> t' -> t') -> t' -> FreeList t a -> t'
foldrFreeList f tb lst = foldr f tb (toList lst)

traverseFreeList :: Applicative f => (t -> f t') -> FreeList t () -> f (FreeList t' ()) -- FIXME: current implementation discards the result
traverseFreeList f lst = fmap fromList . traverse f $ toList lst


-- === Instances === --

instance PrimMonad m => PrimMonad (FreeListT t m) where
  type PrimState (FreeListT t m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

type instance Item (FreeList t a) = t
instance t~s => Convertible (FreeList t a) [s] where
  convert = toList . unwrap where
    toList :: Free (ListCons t) a -> [t]
    toList (FreeT (Identity f)) = case f of
      Free (ListCons t f) -> t : toList f
      Pure _              -> mempty

instance (t~s, Monoid a) => Convertible [s] (FreeList t a) where
  convert = wrap . fromList where
    fromList :: [t] -> Free (ListCons t) a
    fromList = FreeT . Identity . \case
      []     -> Pure mempty
      (t:ts) -> Free . ListCons t $ fromList ts

instance Show t => Show (FreeList t a) where show = show . toList



--------------------
-- === Color === ---
--------------------

data Tone
  = RGB Rational Rational Rational



------------------------
-- === CSS Values === --
------------------------

-- === Definition === --

newtype Unit    = Unit Text deriving (Ord, Eq)
type    UnitMap = Map Unit Int

makeLenses ''Unit

em, ex, pct, px, cm, mm, in', pt, pc, ch, rem, vh, vw, vmin, vmax :: Unit
em      = Unit #em
ex      = Unit #ex
pct     = Unit "%"
px      = Unit #px
cm      = Unit #cm
mm      = Unit #mm
in'     = Unit #in
pt      = Unit #pt
pc      = Unit #pc
ch      = Unit #ch
rem     = Unit #rem
vh      = Unit #vh
vw      = Unit #vw
vmin    = Unit #vmin
vmax    = Unit #vmax


instance Show Unit where show = show . unwrap
instance {-# OVERLAPPABLE #-} Convertible a Text => Convertible a    Unit where convert = convertVia @Text
instance {-# OVERLAPPABLE #-} Convertible Text a => Convertible Unit a    where convert = convertVia @Text
instance                                            Convertible Text Unit where convert = coerce
instance                                            Convertible Unit Text where convert = coerce
instance IsString Unit where fromString = convert

data Number = Number
  { _unitMap :: UnitMap
  , _rawNum  :: Rational
  } deriving (Eq)
makeLenses ''Number

instance Show Number where
  showsPrec d (Number u r) = showParen' d
    $ showString "Number "
    . showsPrec' (toList u)
    . showString " "
    . showsPrec' (convertTo @Double r)

instance Num Number where
  fromInteger               = Number mempty . fromInteger
  Number u a * Number u' a' = Number (Map.unionWith (+) u u') (a * a')
  Number u a + Number u' a' = if u == u' then Number u (a + a') else error $ "Cannot add numbers with different units " <> show u <> " and " <> show u'
  Number u a - Number u' a' = if u == u' then Number u (a + a') else error $ "Cannot subtract numbers with different units " <> show u <> " and " <> show u'
  abs    (Number u a)       = Number u (abs a)
  signum (Number _ a)       = Number mempty (signum a)

instance Ord Number where
  compare = compare `on` view rawNum -- FIXME: Handle units

instance Real Number where
  toRational (Number _ a) = toRational a

instance Enum Number where
  fromEnum = fromEnum . view rawNum
  toEnum   = Number mempty . toEnum

instance Fractional Number where
  fromRational = Number mempty
  Number u a / Number u' a' = Number u (a / a') -- FIXME: Handle units


instance RealFrac Number where
  properFraction (Number u a) = Number u <$> properFraction a -- FIXME: We lose information about units here
  truncate                    = truncate . view rawNum -- FIXME: We lose information about units here
  round                       = round    . view rawNum -- FIXME: We lose information about units here
  ceiling                     = ceiling  . view rawNum -- FIXME: We lose information about units here
  floor                       = floor    . view rawNum -- FIXME: We lose information about units here


class RealFrac2 a where
  round2 :: a -> a

instance RealFrac2 Number where
  round2 = rawNum %~ convert @Integer . round



-- instance Integral Number where
--   quot (Number u a) (Number u' a') = Number u (quot a a') -- FIXME: Handle units

instance Mempty    Number where mempty = 0
instance Semigroup Number where (<>)   = (+)
instance P.Monoid  Number where mempty  = mempty
                                mappend = mappend



data ValFlag = FlagImportant deriving (Eq, Ord, Show)

important :: ValFlag
important = FlagImportant


newtype Thunk = Thunk Int deriving (Num, Show)
type ThunkMap = IntMap (Val Thunk)

instance Convertible Int Thunk where convert = coerce
instance Convertible Thunk Int where convert = coerce


type MonadThunk a = MonadState ThunkMap a


data RawVal a
  = Var Text
  | Num Number
  | Txt Text
  | App Text [a]
  deriving (Foldable, Functor, Show, Traversable)

data Val a = Val
  { _valFlags :: Set ValFlag
  , _rawVal   :: RawVal a
  } deriving (Foldable, Functor, Traversable)

deriveShow1 ''RawVal
deriveShow1 ''Val
makeLenses ''Val
makeLenses ''Thunk


-- FIXME: refactor
newtype Evaluators m = Evaluators [Val Thunk -> m (Val Thunk)]
makeLenses ''Evaluators


instance Convertible Number (RawVal a) where convert = Num

instance {-# OVERLAPPABLE #-} Convertible t (RawVal a)
             => Convertible t          (Val a) where convert = convertVia @(RawVal a)
instance t~a => Convertible (RawVal t) (Val a) where convert = Val mempty


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


fixThunk :: MonadThunk m => Thunk -> m (Fix Val)
fixThunk t = do
  val <- readThunk t
  Fix <$> mapM fixThunk val


instance Show a => Show (Val a) where
  showsPrec d (Val f r) = showParen' d
    $ showString "Val "
    . showsPrec' (toList f)
    . showString " "
    . showsPrec' r

-- data Expr
--   = ExprVal   Val
--   | ExprThunk Thunk



-- (!) :: Val -> ValFlag -> Val
-- (!) v f = v & valFlags %~ Set.insert f

data Def a = Def
  { _name :: Text
  , _val  :: a
  } deriving (Foldable, Functor, Traversable)

fixDef :: MonadThunk m => Def Thunk -> m (Def (Fix Val))
fixDef = mapM fixThunk

instance MonadThunk m => Num (Unit -> StyleValueT Thunk m Thunk) where
  fromInteger = number .: fromInteger

instance Num (Unit -> Number) where
  fromInteger i = flip Number (fromInteger i) . flip Map.singleton 1

-- === Utils === --

-- tryToNumber :: Val -> Maybe Number
-- tryToNumber v = case v ^. rawVal of
--   ValNum a -> Just a
--   _        -> Nothing

data Evaluator = Evaluator { _computedThunks :: Set Thunk } deriving (Show)

-- computeThunks :: (MonadState Evaluator m, MonadThunk m) => m ()
-- computeThunks = do
--   tm <- get @ThunkMap
--   IntMap.keys
--
--
-- while there are unmarked nodes do
--     select an unmarked node n
--     visit(n)

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

evalThunks :: (MonadThunk m, n ~ GetEvaluationMonad m, StateData Evaluators m ~ Evaluators n, MonadState Evaluators m, MonadEvaluation m, Monad n) => m ()
evalThunks = do
  thunks <- getSortedThunks
  mapM_ evalThunk thunks

evalThunk :: (MonadThunk m, n ~ GetEvaluationMonad m, StateData Evaluators m ~ Evaluators n, MonadState Evaluators m, MonadEvaluation m, Monad n) => Thunk -> m ()
evalThunk t = do
  evf <- runEvaluators <$> get @Evaluators
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

-- type MonadEvaluators m = (MonadState Evaluators m, StateData Evaluators m ~ Evaluators m)


runEvaluators :: (MonadEvaluation m, Monad (GetEvaluationMonad m)) => Evaluators (GetEvaluationMonad m) -> (Val Thunk -> m (Val Thunk))
runEvaluators (Evaluators fs) val = liftEvaluation $ foldM (flip ($)) val fs

type family GetEvaluationMonad m where
  GetEvaluationMonad (StateT (Evaluators _) m) = m
  GetEvaluationMonad (t m) = GetEvaluationMonad m

class Monad m => MonadEvaluation m where
  liftEvaluation :: GetEvaluationMonad m a -> m a

instance Monad m => MonadEvaluation (StateT (Evaluators n) m) where liftEvaluation = lift

registerEvaluator :: (StateData Evaluators m ~ Evaluators n, MonadState Evaluators m) => (Val Thunk -> n (Val Thunk)) -> m ()
registerEvaluator f = modify_ @Evaluators (wrapped %~ (<> [f]))
-- Monad m => (a -> b -> m a) -> a -> [b] -> m a
--                                    [Thunk]

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


-- === Instances === --

instance Show a => Show (Def a) where
  showsPrec d (Def n v) = showParen (d > up_prec)
    $ showsPrec (up_prec + 1) n
    . showString " := "
    . showsPrec (up_prec + 1) v
    where up_prec = 5

-- instance IsString Val where
--   fromString = txt . fromString

-- instance Convertible Text Val where convert = txt

-- instance Num Val where
--   fromInteger = number . fromInteger
--   (+)         = app2 "+"
--   (-)         = app2 "-"
--   (*)         = app2 "*"
--   abs         = app1 "abs"
--   signum      = app1 "signum"

-- instance Convertible Number Val where convert = number

-- instance Fractional Val where
--   fromRational = number . fromRational
--   (/)          = app2 "/"
--
-- instance RealFrac2 Val where
--   round2 = app1 "round"


  ----------------------
  -- === Renderer === --
  ----------------------




----------------------------
-- === CSS structures === --
----------------------------

-- === Definition === --

type MonadStyle = MonadFreeList (Decl Thunk)

type    Style  v     = StyleT v Identity
newtype StyleT v m a = StyleT (FreeListT (Decl v) m a) deriving (Functor, Applicative, Monad, MonadFree (ListCons (Decl v)), MonadTrans)
newtype ValueT   m a = ValueT (IdentityT m a)          deriving (Functor, Applicative, Monad, MonadTrans)

type StyleValueT v m = ValueT (StyleT v m)

type ThunkDecl = Decl Thunk
type ValueDecl = Decl (Fix Val)
data Decl v
  = DefDecl     (Def     v)
  | SectionDecl (Section v)
  deriving (Show)

data Selector
  = SimpleSelector Text
  | SubSelector    Selector Selector
  deriving (Show)

data Section v = Section
  { _selector :: Selector
  , _body     :: Style v ()
  }

deriving instance Show v => Show (Section v)




-- === Utils === --

runValueT :: ValueT m a -> m a
runValueT = runIdentityT . unwrap

embedDecl :: Monad m => StyleT v m (Decl v) -> StyleT v m ()
embedDecl d = d >>= liftToFreeList

embedSectionDecl :: Monad m => StyleT v m (Section v) -> StyleT v m ()
embedSectionDecl = embedDecl . fmap SectionDecl

joinStyleT :: Monad m => StyleT v m a -> m (Style v a)
joinStyleT = fmap wrap . joinFreeListT . unwrap

fixDecl :: MonadThunk m => ThunkDecl -> m ValueDecl
fixDecl = mapM fixThunk



class                         (Monad m)                       => AutoAssignment t   m where assignM :: t -> ValueT m Thunk -> m ()
instance {-# OVERLAPPABLE #-} (Monad m, MonadStyle m, t~Text) => AutoAssignment t   m where assignM t v = liftToFreeList =<< runValueT (DefDecl . Def t <$> v)
instance {-# INCOHERENT #-}   (AutoAssignment t m)            => AutoAssignment [t] m where assignM t v = sequence_ $ (flip assignM v) <$> t

assign :: AutoAssignment t m => t -> Thunk -> m ()
assign t v = assignM t (pure v)

infixl 0 =:
(=:) :: AutoAssignment t m => t -> ValueT m Thunk -> m ()
(=:) = assignM



-- === Instances === --

instance IsString Selector where
  fromString = SimpleSelector . fromString

instance {-# OVERLAPPABLE #-} (s ~ StyleT v (StyleT v m) (), a ~ (), Monad m) => IsString (s -> StyleT v m a) where
  fromString sel sect = embedSectionDecl (Section (fromString sel) <$> joinStyleT sect)

instance Rewrapped (StyleT v m a) (StyleT v' m a)
instance Wrapped   (StyleT v m a) where
  type Unwrapped   (StyleT v m a) = FreeListT (Decl v) m a
  _Wrapped' = iso (\(StyleT a) -> a) StyleT

deriving instance Show (Unwrapped (StyleT v m a)) => Show (StyleT v m a)

type instance Item (StyleT v m a) = Item (Unwrapped (StyleT v m a))
instance v~v' => Convertible (StyleT v Identity ()) [Decl v'] where
  convert = convert . unwrap

instance PrimMonad m => PrimMonad (StyleT v m) where
  type PrimState (StyleT v m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (ValueT m) where
  type PrimState (ValueT m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

makeLenses ''Section
makeLenses ''ValueT

instance Functor     Section where fmap  f = (body . wrapped) %~ mapFreeListT (fmap f)
instance Foldable    Section -- FIXME TODO where foldr f t = foldrFreeList f t . view (body . wrapped)
instance Traversable Section where traverse f = (body . wrapped) $ traverseFreeList (traverse f)

deriving instance Functor Decl
deriving instance Foldable Decl
deriving instance Traversable Decl


instance MonadThunk m => Num (StyleValueT Thunk m Thunk) where
  fromInteger = number . fromInteger
  (+)         = appM2 "+"
  (-)         = appM2 "-"
  (*)         = appM2 "*"
  abs         = appM1 "abs"
  signum      = appM1 "signum"

instance MonadThunk m => Fractional (StyleValueT Thunk m Thunk) where
  fromRational = number . fromRational
  (/)          = appM2 "/"

  -- number :: MonadThunk m => Number -> m Thunk

----------------------
-- === Renderer === --
----------------------

-- ==== Definition === --

class Renderer style t where
  render :: [ValueDecl] -> Text


-- === Styles === --

data Compact
data Pretty
