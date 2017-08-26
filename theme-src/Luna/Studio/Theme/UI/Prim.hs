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
accentColorWide    = var "accent-color-wide"
accentColorSubtle  = var "accent-color-subtle"
accentBgLayerColor = var "accent-bg-layer-color"
bgColor            = var "base-background-color"
syntaxBgColor      = var "syntax-background-color"


-------------------
-- === Fonts === --
-------------------

data Font = Font { _size :: Expr }
makeLenses ''Font


-------------------------
-- === Look & Feel === --
-------------------------

-- === Fonts === --

-- FIXME DEPRECATED: move to sizeOf
fontMap :: Map Text Font
fontMap = fromList
  [ (#base       , Font (12px))
  , (#section    , Font (28px))
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
  , (#packageDescription , uiSize)
  , (#title              , uiSize)
  , (#secondaryInfo      , uiSize)
  , (#option             , uiSize * 1.5)
  , (#description        , uiSize / 2.5)
  , (#inlineControl      , uiSize * 0.9)
  , (#tab                , uiSize)
  , (#input              , base * 0.5)
  ] where base = 20px
uiSize = 12px -- FIXME: remove

marginOf :: HasCallStack => Text -> Expr
marginOf t = marginMap ^?! ix t


-- === Measurements === --

sizeMap :: Map Text Expr
sizeMap = fromList
  [ (#paneBorder  , 4px)
  , (#tab         , uiSize * 3)
  , (#text        , 12px)
  , (#title       , 13px)
  , (#heading     , 16px)
  , (#section     , 16px)
  , (#description , 12px)
  , (#line        , uiSize * 2)
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

-- FIXME: move to roundness some elems
radiusMap :: Map Text Expr
radiusMap = fromList
  [ (#button  , 8px)
  , (#card    , 40px)
  , (#message , 15px)
  ]

radiusOf :: HasCallStack => Text -> Expr
radiusOf t = radiusMap ^?! ix t


-- === Colors === --

colorMap :: Map Text Expr
colorMap = fromList
  [ (#text          , rgba 1 1 1 0.6)
  , (#layer         , rgba 1 1 1 0.05)
  , (#toggle        , rgba 1 1 1 0.14)
  , (#border        , rgba 0 0 0 1)
  , (#panelHighlight, "mix" accentColor black 50)
  ]

colorOf, bakedColorOf :: HasCallStack => Text -> Expr
colorOf      t = colorMap ^?! ix t
bakedColorOf t = colorOf t `flatOver` bgColor

ctxColorAlpha = 0.7 -- FIXME: think how to represent context colors


-- === Color modificators === --

subtle, secondary :: Expr -> Expr
subtle    = modAlpha (* 0.4)
secondary = modAlpha (* 0.5)

hover, selected, highlighted, disabled, inactive :: Expr -> Expr
hover       = modAlpha (+ 0.04)
selected    = modAlpha (+ 0.08)
highlighted = modAlpha (+ 0.08)
disabled    = modAlpha (* 0.5)
inactive    = modAlpha (* 0.5)


-- === Animations === --

animSpeedMap :: Map Text Expr
animSpeedMap = fromList
  [ (#tabCloseIcon    , 0.1s)
  , (#textColorChange , 0.3s)
  , (#minimapHover    , 0.2s)
  , (#tabSwitch       , 0.3s)
  , (#paneDim         , 0.5s)
  ]

animSpeedOf :: HasCallStack => Text -> Expr
animSpeedOf t = animSpeedMap ^?! ix t
