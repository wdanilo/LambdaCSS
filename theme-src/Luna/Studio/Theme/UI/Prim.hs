module Luna.Studio.Theme.UI.Prim where

import Prologue

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Luna.Studio.Theme.UI.Utils


-------------------------------
-- === Raw Less bindings === --
-------------------------------

-- TODO: majority of the bindings should be removed and re-implemented here
accentColor        = var "accent-color"
accentColorSubtle  = var "accent-color-subtle"
accentBgLayerColor = var "accent-bg-layer-color"
bgColor            = var "base-background-color"


-------------------
-- === Fonts === --
-------------------

data Font = Font { _size :: Expr }
makeLenses ''Font


-------------------------
-- === Look & Feel === --
-------------------------

-- === Fonts === --

fontMap :: Map Text Font
fontMap = fromList
  [ (#base       , Font (12px))
  , (#section    , Font (36px))
  , (#description, Font (13px))
  ]

fontOf :: HasCallStack => Text -> Font
fontOf t = fontMap ^?! ix t

fontSizeOf :: HasCallStack => Text -> Expr
fontSizeOf t = fontOf t ^. size


-- === Margins === --

marginMap :: Map Text Expr
marginMap = fromList
  [ (#base               , base)
  , (#panel              , base * 2)
  , (#item               , base)
  , (#subSection         , uiSize * 5)
  , (#sectionSide        , uiSize * 4)
  , (#sectionDesc        , uiSize * 1.5)
  , (#sectionBody        , uiSize * 3)
  , (#description        , uiSize)
  , (#title              , uiSize)
  , (#secondaryInfo      , uiSize)
  , (#option             , uiSize * 1.5)
  , (#optionDescription  , uiSize / 2.5)
  , (#inlineControl      , uiSize * 0.9)
  ] where base = 20px
uiSize = 12px -- FIXME: remove

marginOf :: HasCallStack => Text -> Expr
marginOf t = marginMap ^?! ix t


-- === Measurements === --

sizeMap :: Map Text Expr
sizeMap = fromList
  [ (#paneBorder, 4px)
  ]

sizeOf :: HasCallStack => Text -> Expr
sizeOf t = sizeMap ^?! ix t


-- === Roundness == --

roundnessMap :: Map Text Expr
roundnessMap = fromList
  [ (#base               , base)
  , (#switch             , base)
  ] where base = 1

roundnessOf :: HasCallStack => Text -> Expr
roundnessOf t = roundnessMap ^?! ix t

-- FIXME: move to roundness
radiusMap :: Map Text Expr
radiusMap = fromList
  [ (#button , 8px)
  , (#card   , 40px)
  ]

radiusOf :: HasCallStack => Text -> Expr
radiusOf t = radiusMap ^?! ix t


-- === Colors === --

colorMap :: Map Text Expr
colorMap = fromList
  [ (#text   , rgba 1 1 1 0.6)
  , (#layer  , rgba 1 1 1 0.05)
  , (#toggle , rgba 1 1 1 0.14)
  , (#border , rgba 0 0 0 1)
  ]

colorOf, bakedColorOf :: HasCallStack => Text -> Expr
colorOf      t = colorMap ^?! ix t
bakedColorOf t = colorOf t `flatOver` bgColor


-- === Color modificators === --

subtle, secondary :: Expr -> Expr
subtle    = modAlpha (* 0.4)
secondary = modAlpha (* 0.5)

hover, selected, highlighted, disabled :: Expr -> Expr
hover       = modAlpha (+ 0.04)
selected    = modAlpha (+ 0.08)
highlighted = modAlpha (+ 0.08)
disabled    = modAlpha (* 0.5)
