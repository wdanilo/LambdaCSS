{-# LANGUAGE UndecidableInstances #-}

module Data.Color where

import Prologue hiding (Index)
import GHC.Exts
import Unsafe.Coerce

import Type.List
import Type.Error


-----------------------------------
-- === Simple hetero records === --
-----------------------------------

-- === Definition === --

data Record (conses :: [*]) = Record !Int !Any
instance Show (Record s) where show _ = "Record"


-- === Conversion Cons -> Record === --

instance (midx ~ Index a s, ConsToRecord midx)
      => Convertible a (Record s) where convert = consToRecord @midx

class ConsToRecord (midx :: Maybe Nat) where
  consToRecord :: forall a s. a -> Record s

instance KnownNat idx => ConsToRecord ('Just idx) where
  consToRecord a = Record (fromIntegral $ fromType @idx) (unsafeCoerce a)


-- === Conversion Record -> Cons === --

instance (RecordToCons 0 s a)
      => Convertible (Record s) a where convert = recordToCons @0 @s

class RecordToCons (cidx :: Nat) s a where
  recordToCons :: Record s -> a

instance (Convertible' s a, RecordToCons nidx ss a, KnownNat idx, nidx ~ (idx + 1))
      => RecordToCons idx (s ': ss) a where
  recordToCons (Record idx a) = if fromIntegral (fromType @idx) == idx
    then convert' (unsafeCoerce a :: s)
    else recordToCons @nidx @ss (Record idx a)

instance RecordToCons idx '[] a where recordToCons _ = impossible



--------------------
-- === Color === ---
--------------------

-- === Definition === --

newtype Color a = Color (ColorDefinition a)
type family ColorDefinition a
makeLenses ''Color


-- === Instances === --

deriving instance Show (Unwrapped (Color a)) => Show (Color a)


-- === Basic color spaces === --

data RGB
data ColorRGB = ColorRGB { __r :: !Double, __g :: !Double, __b :: !Double, __a :: !Double } deriving (Show)
type instance ColorDefinition RGB = ColorRGB
makeLenses ''ColorRGB

data HSL
data ColorHSL = ColorHSL { __h :: !Double, __s :: !Double, __l :: !Double, __a :: !Double } deriving (Show)
type instance ColorDefinition HSL = ColorHSL
makeLenses ''ColorHSL

rgb :: Convertible' (Color RGB) color => Double -> Double -> Double -> color
rgb r g b = convert' . Color @RGB $ ColorRGB r g b 1


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
