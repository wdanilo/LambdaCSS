{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Class where

import qualified Prelude as P
import Prologue hiding ((>))
-- import Control.Monad.Free

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Control.Lens       as Lens
import Prelude (round)


import Control.Monad.Trans.Free hiding (wrap)

instance (PrimMonad m, Functor a) => PrimMonad (FreeT a m) where
  type PrimState (FreeT a m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}



class IsSubSelector a b where
  infixr 4 >
  (>) :: a -> a -> b

instance {-# OVERLAPPABLE #-} Ord a => IsSubSelector a Bool where
  (>) = (P.>)



----------------------
-- === FreeList === --
----------------------

-- === Definition === --

type    FreeList  t     = FreeListT t Identity
newtype FreeListT t m a = FreeListT (FreeT (ListCons t) m a) deriving (Applicative, Functor, Monad, MonadFree (ListCons t), MonadTrans)

data ListCons a next = ListCons a next
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''FreeListT


-- === Utils === --

liftToFreeList :: MonadFree (ListCons a) m => a -> m ()
liftToFreeList a = liftF $ ListCons a ()


-- === Instances === --

instance PrimMonad m => PrimMonad (FreeListT t m) where
  type PrimState (FreeListT t m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

type instance Item (FreeList t a) = t
instance t~s => Convertible (FreeList t a) [s] where
  convert = toList . unwrap where
    toList :: Free (ListCons t) a -> [t]
    toList (FreeT (Identity f)) = case f of
      Pure _ -> mempty
      Free (ListCons t f) -> t : toList f

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

em, ex, percent, px, cm, mm, in', pt, pc, ch, rem, vh, vw, vmin, vmax :: Unit
em      = Unit #em
ex      = Unit #ex
percent = Unit #percent
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


instance Show Unit where show = convert . unwrap

data Number = Number
  { _unitMap :: UnitMap
  , _rawNum  :: Rational
  } deriving (Show, Eq)
makeLenses ''Number

instance Num Number where
  fromInteger               = Number mempty . fromInteger
  Number u a * Number u' a' = Number (Map.unionWith (+) u u') (a * a')
  Number u a + Number u' a' = if u == u then Number u (a + a') else error $ "Cannot add numbers with different units " <> show u <> " and " <> show u'
  Number u a - Number u' a' = if u == u then Number u (a + a') else error $ "Cannot subtract numbers with different units " <> show u <> " and " <> show u'
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


data RawVal
  = ValVar Text
  | ValNum Number
  | ValTxt Text
  | ValApp Text [Val]
  deriving (Show)

data Val = Val
  { _valFlags :: Set ValFlag
  , _rawVal   :: RawVal
  } deriving (Show)

makeLenses ''Val

data Expr
  = ExprVal   Val
  | ExprThunk Thunk

newtype Thunk = Thunk Int deriving (Num, Show)
type ThunkMap = IntMap Thunk

(!) :: Val -> ValFlag -> Val
(!) v f = v & valFlags %~ Set.insert f

data Def = Def
  { _name :: Text
  , _val  :: Val
  }


instance Num (Unit -> Val) where
  fromInteger = number .: fromInteger

instance Num (Unit -> Number) where
  fromInteger i = flip Number (fromInteger i) . flip Map.singleton 1

-- === Utils === --

tryToNumber :: Val -> Maybe Number
tryToNumber v = case v ^. rawVal of
  ValNum a -> Just a
  _        -> Nothing


val :: RawVal -> Val
val = Val mempty

var    :: Text   -> Val
number :: Number -> Val
txt    :: Text   -> Val
app    :: Text -> [Val] -> Val
var    = val .  ValVar
number = val .  ValNum
txt    = val .  ValTxt
app    = val .: ValApp

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

app1 :: Text -> Val -> Val
app2 :: Text -> Val -> Val -> Val
app3 :: Text -> Val -> Val -> Val -> Val
app1 n t1       = app n [t1]
app2 n t1 t2    = app n [t1, t2]
app3 n t1 t2 t3 = app n [t1, t2, t3]


-- data Thunks = Map Text Value


-- === Instances === --

instance Show Def where
  showsPrec d (Def n v) = showParen (d > up_prec)
    $ showsPrec (up_prec + 1) n
    . showString " := "
    . showsPrec (up_prec + 1) v
    where up_prec = 5

instance IsString Val where
  fromString = txt . fromString

instance Num Val where
  fromInteger = number . fromInteger
  (+)         = app2 "+"
  (-)         = app2 "-"
  (*)         = app2 "*"
  abs         = app1 "abs"
  signum      = app1 "signum"

instance Convertible Number Val where convert = number

instance Fractional Val where
  fromRational = number . fromRational
  (/)          = app2 "/"

instance RealFrac2 Val where
  round2 = app1 "round"

----------------------------
-- === CSS structures === --
----------------------------

-- === Definition === --

type    SectionBody    = SectionBody' ()
newtype SectionBody' a = SectionBody (FreeList Decl a) deriving (Show, Functor, Applicative, Monad)

data Decl = DefDecl     Def
          | SectionDecl Section
          deriving (Show)

data Selector
  = SimpleSelector Text
  | SubSelector    Selector Selector
  deriving (Show)

data Section = Section
  { _selector :: Selector
  , _body     :: SectionBody
  } deriving (Show)

makeLenses ''Section
-- Lens.makeWrapped ''SectionBody


-- === Utils === --

decl :: Decl -> SectionBody
decl = SectionBody . liftToFreeList

sectionDecl :: Section -> SectionBody
sectionDecl = decl . SectionDecl

infixl 0 :=
pattern (:=) :: Text -> Val -> SectionBody
pattern (:=){t, v} = SectionBody (FreeListT (FreeT (Identity (Free (ListCons (DefDecl (Def t v)) (FreeT (Identity (Pure ()))))))))

infixl 0 %=
(%=) :: [Text] -> Val -> SectionBody
ts %= v = sequence_ $ (:= v) <$> ts




-- === Instances === --

instance IsString Selector where
  fromString = SimpleSelector . fromString

instance IsString (SectionBody -> Section) where
  fromString = Section . fromString

instance a ~ () => IsString (SectionBody -> SectionBody' a) where
  fromString s = sectionDecl . Section (fromString s)

instance (a ~ Selector)         => IsSubSelector a Selector                        where (>) = SubSelector
instance (a ~ Selector)         => IsSubSelector a (SectionBody -> Section)        where (>) = Section .: (>)
instance (a ~ Selector, t ~ ()) => IsSubSelector a (SectionBody -> SectionBody' t) where (>) = (sectionDecl .: Section) .: (>)

instance Wrapped (SectionBody' a) where
  type Unwrapped (SectionBody' a) = FreeList Decl a
  _Wrapped' = iso (\(SectionBody a) -> a) SectionBody

type instance Item SectionBody = Decl
instance Convertible SectionBody [Decl] where
  convert = convert . unwrap


----------------------
-- === Renderer === --
----------------------

-- ==== Definition === --

class Renderer style t where
  render :: [Decl] -> Text


-- === Styles === --

data Compact
data Pretty
