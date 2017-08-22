{-# LANGUAGE UndecidableInstances #-}

module Data.Color where

import Prologue hiding (Index)
import GHC.Exts
import Unsafe.Coerce

import Type.List
import Type.Error
import Data.Hashable


-----------------------------------
-- === Simple hetero records === --
-----------------------------------

-- === Definition === --

data Record (conses :: [*]) = Record !Int !Int !Any
instance Show (Record s) where show _ = "Record"


-- === Conversion Cons -> Record === --

instance (midx ~ Index a s, ConsToRecord midx, Hashable a)
      => Convertible a (Record s) where convert = consToRecord @midx

class ConsToRecord (midx :: Maybe Nat) where
  consToRecord :: forall a s. Hashable a => a -> Record s

instance KnownNat idx => ConsToRecord ('Just idx) where
  consToRecord a = Record (hash a) (fromIntegral $ fromType @idx) (unsafeCoerce a)


-- === Conversion Record -> Cons === --

instance (RecordToCons 0 s a)
      => Convertible (Record s) a where convert = recordToCons @0 @s

class RecordToCons (cidx :: Nat) s a where
  recordToCons :: Record s -> a

instance (Convertible' s a, RecordToCons nidx ss a, KnownNat idx, nidx ~ (idx + 1))
      => RecordToCons idx (s ': ss) a where
  recordToCons (Record i idx a) = if fromIntegral (fromType @idx) == idx
    then convert' (unsafeCoerce a :: s)
    else recordToCons @nidx @ss (Record i idx a)

instance RecordToCons idx '[] a where recordToCons _ = impossible


-- === Instances === --

instance Hashable (Record a) where
  hashWithSalt _ (Record i _ _) = i



--------------------
-- === Color === ---
--------------------

-- === Definition === --

newtype Color a = Color (ColorDefinition a)
type family ColorDefinition a
makeLenses ''Color


-- === Instances === --

deriving instance Show     (Unwrapped (Color a)) => Show     (Color a)
deriving instance Hashable (Unwrapped (Color a)) => Hashable (Color a)


-- === Basic color spaces === --

data RGB
data ColorRGB = ColorRGB { __r :: !Double, __g :: !Double, __b :: !Double, __a :: !Double } deriving (Generic, Show)
type instance ColorDefinition RGB = ColorRGB
makeLenses ''ColorRGB

data HSL
data ColorHSL = ColorHSL { __h :: !Double, __s :: !Double, __l :: !Double, __a :: !Double } deriving (Generic, Show)
type instance ColorDefinition HSL = ColorHSL
makeLenses ''ColorHSL

instance Hashable ColorRGB
instance Hashable ColorHSL

rgb :: Convertible' (Color RGB) color => Double -> Double -> Double -> color
rgb r g b = rgba r g b 1

rgba :: Convertible' (Color RGB) color => Double -> Double -> Double -> Double -> color
rgba r g b a = convert' . Color @RGB $ ColorRGB r g b a


instance Convertible (Color HSL) (Color RGB) where convert = error "todo"

class HasComponent (s :: Symbol) c where
  component :: Lens' c Double

type        DefaultComponentColor (s :: Symbol) = Color (DefaultComponentSpace s)
type family DefaultComponentSpace (s :: Symbol)

instance {-# OVERLAPPABLE #-} (BiConvertible c c', c' ~ DefaultComponentColor p, HasComponent p c')
      => HasComponent p c where component = convertedTo @c' . component @p

type instance DefaultComponentSpace "r" = RGB
type instance DefaultComponentSpace "g" = RGB
type instance DefaultComponentSpace "b" = RGB
type instance DefaultComponentSpace "a" = RGB -- FIXME[WD]: it should not behave like this

instance HasComponent "r" (Color RGB) where component = wrapped . colorRGB_r
instance HasComponent "g" (Color RGB) where component = wrapped . colorRGB_g
instance HasComponent "b" (Color RGB) where component = wrapped . colorRGB_b
instance HasComponent "a" (Color RGB) where component = wrapped . colorRGB_a

r :: HasComponent "r" a => Lens' a Double
g :: HasComponent "g" a => Lens' a Double
b :: HasComponent "b" a => Lens' a Double
a :: HasComponent "a" a => Lens' a Double
r = component @"r"
g = component @"g"
b = component @"b"
a = component @"a"

--
-- instance Convertible (Record ts) a => Convertible (Record ts) (Tone a) where convert = wrap . convert . unwrap
-- instance Convertible a (Record ts) => Convertible (Tone a) (Record ts) where convert = wrap . convert . unwrap



-- === Conversion Record -> Color === --



-- instance (midx ~ Index a s, RecordToCons midx a s)
--       => Convertible a (Record s) where convert = recordToCons @midx




-- data Record
--   = RGB Rational Rational Rational
--   | HSV Rational Rational Rational
--   | HSL Rational Rational Rational
--   deriving (Show)
--
-- r,g,b,h,s,v :: Int
-- r = 1
-- g = 1
-- b = 1
-- h = 1
-- s = 1
-- v = 1
