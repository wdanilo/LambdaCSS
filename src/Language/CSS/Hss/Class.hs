{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Class where

import qualified Prelude as P
import Prologue hiding ((>))
import Control.Monad.Free

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Control.Lens    as Lens


class IsSubSelector a b where
  infixr 4 >
  (>) :: a -> a -> b

instance {-# OVERLAPPABLE #-} Ord a => IsSubSelector a Bool where
  (>) = (P.>)



----------------------
-- === FreeList === --
----------------------

-- === Definition === --

newtype FreeList t a = FreeList (Free (ListCons t) a)
  deriving (Functor, Applicative, Monad)

data ListCons a next = ListCons a next
  deriving (Show, Functor)

makeLenses ''FreeList


-- === Utils === --

liftToFreeList :: a -> FreeList a ()
liftToFreeList a = FreeList $ liftF (ListCons a ())


-- === Instances === --

type instance Item (FreeList t a) = t
instance t~s => Convertible (FreeList t a) [s] where
  convert = toList . unwrap where
    toList :: Free (ListCons t) a -> [t]
    toList = \case
      Pure _ -> mempty
      Free (ListCons t f) -> t : toList f

instance Show t => Show (FreeList t a) where show = show . toList



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

data Number = Number UnitMap Double deriving (Show)

instance Num Number where
  fromInteger               = Number mempty . fromInteger
  Number u a * Number u' a' = Number (Map.unionWith (+) u u') (a * a')
  Number u a + Number u' a' = if u == u then Number u (a + a') else error $ "Cannot add numbers with different units " <> show u <> " and " <> show u'
  Number u a - Number u' a' = if u == u then Number u (a + a') else error $ "Cannot subtract numbers with different units " <> show u <> " and " <> show u'
  abs    (Number u a)       = Number u (abs a)
  signum (Number _ a)       = Number mempty (signum a)

instance Mempty    Number where mempty = 0
instance Semigroup Number where (<>)   = (+)
instance P.Monoid  Number where mempty  = mempty
                                mappend = mappend

instance Num (Unit -> Val) where
  fromInteger = ValNum .: fromInteger

instance Num (Unit -> Number) where
  fromInteger i = flip Number (fromInteger i) . flip Map.singleton 1

data Val
  = Var Text
  | ValNum Number
  | Txt Text
  | App Text [Val]
  deriving (Show)

data Def = Def
  { _name :: Text
  , _val  :: Val
  }


-- === Utils === --

tryToNumber :: Val -> Maybe Number
tryToNumber = \case
  ValNum a -> Just a
  _        -> Nothing


var :: Text -> Val
var = Var

numApp :: Text -> ([Number] -> Number) -> [Val] -> Val
numApp n f args = case sequence (tryToNumber <$> args) of
  Just args' -> ValNum $ f args'
  Nothing    -> App n args

numApp1 :: Text -> (Number -> Number)                     -> Val -> Val
numApp2 :: Text -> (Number -> Number -> Number)           -> Val -> Val -> Val
numApp3 :: Text -> (Number -> Number -> Number -> Number) -> Val -> Val -> Val -> Val
numApp1 n f t1       = numApp n (\[s1]         -> f s1)       [t1]
numApp2 n f t1 t2    = numApp n (\[s1, s2]     -> f s1 s2)    [t1, t2]
numApp3 n f t1 t2 t3 = numApp n (\[s1, s2, s3] -> f s1 s2 s3) [t1, t2, t3]


-- === Instances === --

instance Show Def where
  showsPrec d (Def n v) = showParen (d > up_prec)
    $ showsPrec (up_prec + 1) n
    . showString " := "
    . showsPrec (up_prec + 1) v
    where up_prec = 5

instance IsString Val where
  fromString = Txt . fromString

instance Num Val where
  fromInteger = ValNum . fromInteger
  (+)         = numApp2 "+" (+)
  (-)         = numApp2 "-" (-)
  (*)         = numApp2 "*" (*)
  abs         = numApp1 "abs" abs
  signum      = numApp1 "signum" signum

instance Convertible Number Val where convert = ValNum


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
pattern (:=){t, v} = SectionBody (FreeList (Free (ListCons (DefDecl (Def t v)) (Pure ()))))


-- === CSS attributes === --



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
