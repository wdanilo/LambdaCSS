{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.CSS.Hss.Class where

import qualified Prelude as P
import Prologue hiding (assign, properFraction, truncate, round, ceiling, floor)

import           Control.Monad.State.Layered
import           Control.Monad.Free.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Control.Lens                as Lens
import           Data.Color
import           Data.Layout                 (Doc)
import qualified Data.Char                   as Char
import qualified GHC.Exts                    as Lits
import Prelude (round)

import Control.Monad.Trans.Free hiding (wrap)

import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number

instance (PrimMonad m, Functor a) => PrimMonad (FreeT a m) where
  type PrimState (FreeT a m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

type family Eqs  ts   :: Constraint where
  Eqs '[]       = ()
  Eqs (t ': ts) = Eqs' t ts

type family Eqs' t ts :: Constraint where
  Eqs' p '[]       = ()
  Eqs' p (t ': ts) = (p ~ t, Eqs' p ts)


-------------------
-- === Value === --
-------------------

-- === Definition === --

type Value         = ValueScheme Thunk
type FixedValue    = ValueScheme (Fix ValueScheme)
data ValueScheme a = ValueScheme
  { _valFlags :: Set Text
  , _rawVal   :: RawValueScheme a
  } deriving (Foldable, Functor, Traversable)

type RawValue      = RawValueScheme Thunk
type FixedRawValue = RawValueScheme (Fix ValueScheme)
data RawValueScheme a
  = Var Text
  | Num Number
  | Txt Text
  | Col CSSColor
  | App Text [a]
  deriving (Foldable, Functor, Show, Traversable)

type CSSColor = Record '[Color RGB, Color HSL]


-- === Utils === --

simpleValueScheme :: RawValueScheme a -> ValueScheme a
simpleValueScheme = ValueScheme mempty


-- === Flags === --

class HasFlag a where
  addFlag :: Text -> a -> a

(!) :: HasFlag a => a -> Text -> a
(!) = flip addFlag

important :: Text
important = "important"


-- === Instances === --

-- Conversions
instance Convertible Number (RawValueScheme a) where convert = Num
instance {-# OVERLAPPABLE #-} Convertible t (RawValueScheme a)
             => Convertible t                  (ValueScheme a) where convert = convertVia @(RawValueScheme a)
instance t~a => Convertible (RawValueScheme t) (ValueScheme a) where convert = ValueScheme mempty

-- Pretty
instance Show a => Show (ValueScheme a) where
  showsPrec d (ValueScheme f r) = showParen' d
    $ showString "ValueScheme "
    . showsPrec' (toList f)
    . showString " "
    . showsPrec' r



-------------------
-- === Thunk === --
-------------------

newtype Thunk        = Thunk Int deriving (Num, Show)
type    ThunkMap     = IntMap Value
type    MonadThunk a = MonadState ThunkMap a
makeLenses ''Thunk


-- === Construction === --

newThunk :: MonadThunk m => Value -> m Thunk
newThunk v = modify @ThunkMap go where
  go m = (wrap idx, IntMap.insert idx v m) where
    idx = IntMap.size m

readThunk    :: MonadThunk m => Thunk -> m Value
readThunkRaw :: MonadThunk m => Thunk -> m RawValue
readThunk    t = (^?! ix (unwrap t)) <$> get @ThunkMap
readThunkRaw t = (\(ValueScheme _ r) -> r) <$> readThunk t

writeThunk :: MonadThunk m => Thunk -> Value -> m ()
writeThunk t = modifyThunk t . const

modifyThunk :: MonadThunk m => Thunk -> (Value -> Value) -> m ()
modifyThunk t f = modify_ @ThunkMap $ ix (unwrap t) %~ f


-- === Catamorphisms === --

fixThunk :: MonadThunk m => Thunk -> m (Fix ValueScheme)
fixThunk t = do
  val <- readThunk t
  Fix <$> mapM fixThunk val


-- === Instances === --

instance Convertible Int Thunk where convert = coerce
instance Convertible Thunk Int where convert = coerce

makeLenses  ''ValueScheme
deriveShow1 ''ValueScheme
deriveShow1 ''RawValueScheme


------------------------------
-- === Thunk evaluation === --
------------------------------

-- === Definition === --

newtype ThunkPassManager m = ThunkPassManager [Value -> m Value]
makeLenses ''ThunkPassManager


-- === Thunk evaluator base class discovery ===

type family GetThunkEvaluationMonad m where
  GetThunkEvaluationMonad (StateT (ThunkPassManager _) m) = m
  GetThunkEvaluationMonad (t m) = GetThunkEvaluationMonad m

class Monad m => MonadThunkEvaluation m where
  liftThunkEvaluation :: GetThunkEvaluationMonad m a -> m a

instance {-# OVERLAPPABLE #-} (MonadThunkEvaluation m, GetThunkEvaluationMonad (t m) ~ GetThunkEvaluationMonad m, Monad (t m), MonadTrans t)
                 => MonadThunkEvaluation (t m)                         where liftThunkEvaluation = lift . liftThunkEvaluation
instance Monad m => MonadThunkEvaluation (StateT (ThunkPassManager n) m) where liftThunkEvaluation = lift

type MonadThunkSolver m
  = ( StateData ThunkPassManager m ~ ThunkPassManager (GetThunkEvaluationMonad m)
    , MonadState ThunkPassManager m
    , MonadThunkEvaluation m
    , MonadThunk m
    , Monad (GetThunkEvaluationMonad m))


-- === Running === --

evalThunkPassManager :: Functor m => StateT (ThunkPassManager m) m a -> m a
evalThunkPassManager = flip evalStateT (ThunkPassManager mempty)


-- === Construction === --

registerThunkPass :: (StateData ThunkPassManager m ~ ThunkPassManager n, MonadState ThunkPassManager m)
                  => (Value -> n Value) -> m ()
registerThunkPass f = modify_ @ThunkPassManager (wrapped %~ (<> [f]))


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

compileThunkPassManager :: (MonadThunkEvaluation m, Monad (GetThunkEvaluationMonad m))
                      => ThunkPassManager (GetThunkEvaluationMonad m) -> (Value -> m Value)
compileThunkPassManager (ThunkPassManager fs) val = liftThunkEvaluation $ foldM (flip ($)) val fs

evalThunks :: MonadThunkSolver m => m ()
evalThunks = do
  thunks <- getSortedThunks
  mapM_ evalThunk thunks
  where
    evalThunk :: MonadThunkSolver m => Thunk -> m ()
    evalThunk t = do
      evf <- compileThunkPassManager <$> get @ThunkPassManager
      val <- readThunk t
      val' <- evf val
      writeThunk t val'

funcEvaluator :: MonadThunk m => Value -> m Value
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

evalApp :: Monad m => Text -> [Value] -> m (Maybe Value)
evalApp n vs = return $ case (n, view rawVal <$> vs) of
  ("*"     , [Num (Number u a), Num (Number u' a')]) -> Just $ convert $ simplifyUnits $ Number (Map.unionWith (+) u u') (a * a')
  ("/"     , [Num (Number u a), Num (Number u' a')]) -> Just $ convert $ Number (Map.mergeWithKey (\_ x y -> let w = x + y in justIf (not $ w == 0) w) id id u (negate <$> u')) (a / a')
  ("+"     , [Num (Number u a), Num (Number u' a')]) -> justIf (u == u') $ convert $ Number u $ a + a'
  ("round" , [Num (Number u a)])                     -> Just $ convert $ Number u (convert @Int $ P.round a)
  _ -> Nothing


-- === Helpers === --

mkThunkRaw :: MonadThunk m => RawValue -> m Thunk
mkThunkRaw = newThunk . simpleValueScheme

mkVarThunk   :: MonadThunk m => Text     -> m Thunk
mkNumThunk   :: MonadThunk m => Number   -> m Thunk
mkTxtThunk   :: MonadThunk m => Text     -> m Thunk
mkColorThunk :: MonadThunk m => CSSColor -> m Thunk
mkAppThunk   :: MonadThunk m => Text -> [Thunk] -> m Thunk
mkVarThunk   = mkThunkRaw .  Var
mkNumThunk   = mkThunkRaw .  Num
mkTxtThunk   = mkThunkRaw .  Txt
mkAppThunk   = mkThunkRaw .: App
mkColorThunk = mkThunkRaw .  Col

mkAppThunkM :: MonadThunk m => Text -> [m Thunk] -> m Thunk
mkAppThunkM n = mkAppThunk n <=< sequence

mkAppThunkM1 :: MonadThunk m => Text -> m Thunk -> m Thunk
mkAppThunkM2 :: MonadThunk m => Text -> m Thunk -> m Thunk -> m Thunk
mkAppThunkM3 :: MonadThunk m => Text -> m Thunk -> m Thunk -> m Thunk -> m Thunk
mkAppThunkM1 n t1       = mkAppThunkM n [t1]
mkAppThunkM2 n t1 t2    = mkAppThunkM n [t1, t2]
mkAppThunkM3 n t1 t2 t3 = mkAppThunkM n [t1, t2, t3]



------------------------
-- === Definition === --
------------------------

-- === Definition === --

data Def a = Def
  { _name :: Text
  , _val  :: a
  } deriving (Foldable, Functor, Traversable)

--- === Catamorphisms === --

fixDef :: MonadThunk m => Def Thunk -> m (Def (Fix ValueScheme))
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
type ValueDecl = Decl (Fix ValueScheme)

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
instance {-# OVERLAPPABLE #-} (s ~ StyleSchemeT v (StyleSchemeT v m) (), a ~ (), Monad m)
      => IsString (s -> StyleSchemeT v m a) where
  fromString sel sect = embedSectionDecl (Section (fromString sel) <$> joinStyleT sect)

-- | Syntax `#settingsView $ do ...` <=> `.settingsView {...}`
instance (IsString (s -> StyleSchemeT v m a), KnownSymbol lab)
      => IsLabel lab (s -> StyleSchemeT v m a) where
  fromLabel = fromString . ('.':) . fromCamelCase $ fromType @lab where
    fromCamelCase = \case
      []     -> []
      (c:cs) -> (if Char.isUpper c then (\s -> '-' : Char.toLower c : s) else (c:))
              $ fromCamelCase cs

-- instance Lits.IsList  

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



------------------
-- === Expr === --
------------------

-- === Definition === --
-- | The `ExprT` type is defined only to make automatic lifting of literals more type-precise
--   and to awoid obscure inferencer messages.

newtype ExprT      m a = ExprT (IdentityT    m a) deriving (Functor, Applicative, Monad, MonadTrans)
type    StyleExprSchemeT v m = ExprT (StyleSchemeT v m)

-- newtype Style = Style (forall m. MonadThunk m => ...)
newtype Expr  = Expr  (forall m. MonadThunk m => StyleSchemeT Thunk m Thunk)
makeLenses ''ExprT


-- === Running === --

runExprT :: ExprT m a -> m a
runExprT = runIdentityT . unwrap

runExpr :: MonadThunk m => Expr -> StyleSchemeT Thunk m Thunk
runExpr (Expr e) = e


-- === Assignments === --

class Monad m => AutoAssignment t m where
  assignM :: t -> Expr -> m ()

instance {-# OVERLAPPABLE #-}
         (t~Text, m ~ StyleSchemeT Thunk n, MonadThunk n) => AutoAssignment t   m where assignM t expr = liftToFreeList =<< (DefDecl . Def t <$> runExpr expr)
instance {-# INCOHERENT #-} (AutoAssignment t m)          => AutoAssignment [t] m where assignM t v = sequence_ $ (flip assignM v) <$> t

infixl 1 =:
(=:) :: AutoAssignment t m => t -> Expr -> m ()
(=:) = assignM


-- === Constructors === --

mkVarExpr   :: Text     -> Expr
mkNumExpr   :: Number   -> Expr
mkTxtExpr   :: Text     -> Expr
mkColorExpr :: CSSColor -> Expr
mkAppExpr   :: Text -> [Expr] -> Expr
mkVarExpr   a    = Expr $ mkVarThunk   a
mkNumExpr   a    = Expr $ mkNumThunk   a
mkTxtExpr   a    = Expr $ mkTxtThunk   a
mkColorExpr a    = Expr $ mkColorThunk a
mkAppExpr   t as = Expr $ mkAppThunkM t (runExpr <$> as)

mkAppExpr1  :: Text -> Expr -> Expr
mkAppExpr2  :: Text -> Expr -> Expr -> Expr
mkAppExpr3  :: Text -> Expr -> Expr -> Expr -> Expr
mkAppExpr4  :: Text -> Expr -> Expr -> Expr -> Expr -> Expr
mkAppExpr5  :: Text -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
mkAppExpr1 t a1             = mkAppExpr t [a1]
mkAppExpr2 t a1 a2          = mkAppExpr t [a1,a2]
mkAppExpr3 t a1 a2 a3       = mkAppExpr t [a1,a2,a3]
mkAppExpr4 t a1 a2 a3 a4    = mkAppExpr t [a1,a2,a3,a4]
mkAppExpr5 t a1 a2 a3 a4 a5 = mkAppExpr t [a1,a2,a3,a4,a5]


-- === Utils === --

class SemiRealFrac a where
  truncate       :: a -> a
  round          :: a -> a
  ceiling        :: a -> a
  floor          :: a -> a


-- === Literals lifting === --

-- | Syntax `var =: 12`
instance Num Expr where
  fromInteger = mkNumExpr . Number mempty . fromInteger
  (+)         = mkAppExpr2 "+"
  (-)         = mkAppExpr2 "-"
  (*)         = mkAppExpr2 "*"
  abs         = mkAppExpr1 "abs"
  signum      = mkAppExpr1 "signum"

-- | Syntax `var =: 1.5`
instance Fractional Expr where
  fromRational = mkNumExpr . Number mempty
  (/)          = mkAppExpr2 "/"

-- | Syntax `var =: round a`
instance SemiRealFrac Expr where
  truncate = mkAppExpr1 "truncate"
  round    = mkAppExpr1 "round"
  ceiling  = mkAppExpr1 "ceiling"
  floor    = mkAppExpr1 "floor"

-- | Syntax `var =: 12px`
instance Num (Unit -> Expr) where
  fromInteger = mkNumExpr .: fromInteger

-- | Syntax `var =: foo !important`
-- FIXME: Shouldnt we move Flags to AST trans node?
instance HasFlag Expr where
  addFlag f (Expr expr) = Expr $ do
    t <- expr
    modifyThunk t (valFlags %~ Set.insert f)
    return t

-- | Syntax `var =: rgb 1 0 0`
instance Convertible (Color c) CSSColor
      => Convertible (Color c) Expr where
  convert = mkColorExpr . convert'

-- | Syntax `var =: "foo"`
instance IsString Expr where
  fromString = mkTxtExpr . convert

-- | Syntax `var =: "foo" t1 t2 ...`
instance (t~Expr)                       => IsString (t -> Expr)                         where fromString = mkAppExpr1 . convert
instance (t~Expr, Eqs '[t,t2])          => IsString (t -> t2 -> Expr)                   where fromString = mkAppExpr2 . convert
instance (t~Expr, Eqs '[t,t2,t3])       => IsString (t -> t2 -> t3 -> Expr)             where fromString = mkAppExpr3 . convert
instance (t~Expr, Eqs '[t,t2,t3,t4])    => IsString (t -> t2 -> t3 -> t4 -> Expr)       where fromString = mkAppExpr4 . convert
instance (t~Expr, Eqs '[t,t2,t3,t4,t5]) => IsString (t -> t2 -> t3 -> t4 -> t5 -> Expr) where fromString = mkAppExpr5 . convert


-- === Instances === --

instance PrimMonad m => PrimMonad (ExprT m) where
  type PrimState (ExprT m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}



----------------------
-- === Renderer === --
----------------------

-- ==== Definition === --

class Renderer style t where
  render :: [ValueDecl] -> Doc Text


-- === Styles === --

data Compact
data Pretty
