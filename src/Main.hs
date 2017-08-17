{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

import Prologue
import Control.Monad.Free




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

freeCons :: a -> Free (ListCons a) ()
freeCons a = liftF (ListCons a ())


-- === Instances === --

type instance Item (FreeList t a) = t
instance t~s => Convertible (FreeList t a) [s] where
  convert = toList . unwrap where
    toList :: Free (ListCons t) a -> [t]
    toList = \case
      Pure _ -> mempty
      Free (ListCons t f) -> t : toList f

instance Show t => Show (FreeList t a) where show = show . toList



----------------------------
-- === CSS structures === --
----------------------------

-- === Definition === --

type    SectionBody    = SectionBody' ()
newtype SectionBody' a = SectionBody (FreeList Def a) deriving (Show, Functor, Applicative, Monad)

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

pattern (:=) :: Text -> Val -> SectionBody
pattern (:=){t, v} = SectionBody (FreeList (Free (ListCons (Def t v) (Pure ()))))

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


-- === CSS attributes === --

position = "position"
relative = "relative"



---------------- TESTS -----------------


style :: SectionBody
style = do
  position := relative
  position := 12
  position := 12
  position := 12

-- test :: Free (ListCons Int) ()
-- test = do
--   freeCons 1
--   freeCons 1


main :: IO ()
main = do
  pprint style
