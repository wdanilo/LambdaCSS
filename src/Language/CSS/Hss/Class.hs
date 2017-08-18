{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Class where

import qualified Prologue as P
import Prologue hiding ((>))
import Control.Monad.Free

import qualified Control.Lens as Lens


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

data Val
  = Var Text
  | Num Double
  | Txt Text
  | App Text [Val]
  deriving (Show)

data Def = Def
  { _name :: Text
  , _val  :: Val
  }


-- === Utils === --

var :: Text -> Val
var = Var

op :: Text -> Val -> Val -> Val
op n l r = App n [l,r]

app' :: Text -> Val -> Val
app' n a = App n [a]


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
  fromInteger = Num . fromInteger
  (+)         = op "+"
  (-)         = op "-"
  (*)         = op "+"
  abs         = app' "abs"
  signum      = app' "signum"



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

pattern (:=) :: Text -> Val -> SectionBody
pattern (:=){t, v} = SectionBody (FreeList (Free (ListCons (DefDecl (Def t v)) (Pure ()))))


-- === CSS attributes === --

li = "li"

border   = "border"
padding  = "padding"
position = "position"
relative = "relative"


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
