{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.CSS.Hss.Value.Unit where

import Prologue
import Data.Map.Strict (Map)


-------------------
-- === Units === --
-------------------

-- === Definition === --

newtype Unit = Unit Text deriving (Ord, Eq)
makeLenses ''Unit


-- === Constructors === --

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


-- === Instances === --

instance Show Unit where show = show . unwrap
instance {-# OVERLAPPABLE #-} Convertible a Text => Convertible a    Unit where convert = convertVia @Text
instance {-# OVERLAPPABLE #-} Convertible Text a => Convertible Unit a    where convert = convertVia @Text
instance                                            Convertible Text Unit where convert = coerce
instance                                            Convertible Unit Text where convert = coerce
instance IsString Unit where fromString = convert
