module Luna.Studio.Theme.UI.Utils where

import Prologue

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)


-- === Missing properties === --

contentBox = "content-box"
borderBox = "border-box"
pointer = "pointer"
column = "column"
solid = "solid"
auto = "auto"
absolute = "absolute"
scale = "scale"

-- custom
appearance = "-webkit-appearance"
pointerEvents = "pointerEvents"


-- === Basic colors === --

white = rgb 1 1 1
black = rgb 0 0 0


getR, getG, getB :: Expr -> Expr
getR = "red"
getG = "green"
getB = "blue"
getA = "alpha"

flatOver :: Expr -> Expr -> Expr
flatOver top bottom = "rgb" (ta * tr + br) (ta * tg + bg) (ta * tb + bb) where
  tr = getR top
  tg = getG top
  tb = getB top
  ta = getA top
  br = getR bottom
  bg = getG bottom
  bb = getB bottom


alpha :: Expr -> Expr
alpha a = "alpha" a

setAlpha :: Expr -> Expr -> Expr
setAlpha newAlpha t = "fade" t (100 * newAlpha)

modAlpha :: (Expr -> Expr) -> Expr -> Expr
modAlpha f t = setAlpha (f $ alpha t) t

darken :: Expr -> Expr -> Expr
darken = "darken"

lighten :: Expr -> Expr -> Expr
lighten = "lighten"



-- === Generic Utils === --

vcenterChildren = do
  display        =: flex
  flexDirection  =: column
  justifyContent =: center
