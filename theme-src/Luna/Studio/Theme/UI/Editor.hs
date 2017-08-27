module Luna.Studio.Theme.UI.Editor where

import Prologue hiding (none, left)

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Luna.Studio.Theme.UI.Prim
import Luna.Studio.Theme.UI.Utils
import qualified GHC.Exts as Lits


styleEditor :: MonadThunk m => StyleT m ()
styleEditor = do
  styleGitMarkers
  styleMinimap


styleGitMarkers :: MonadThunk m => StyleT m ()
styleGitMarkers = "atom-text-editor .gutter .line-number" $ do
  let clr = setAlpha 0.13 $ colorOf #text
  let styleMarker c = do
        transition =: [color, animSpeedOf #textColorChange]
        borderLeft =: [2px, solid, transparent]
        color      =: setAlpha 0.4 c
        "&.cursor-line" $ do
          transition =: [color, animSpeedOf #textColorChange]
          color =: setAlpha 0.6 c

  opacity    =: 1
  color      =: clr
  marginLeft =: 10px
  transition =: [color, animSpeedOf #textColorChange]

  "&.cursor-line" $ do
    color =: setAlpha 0.5 $ colorOf #text
    transition =: [color, animSpeedOf #textColorChange]

  "&.git-line-added"    $ styleMarker (colorOf #gitAdded)
  "&.git-line-modified" $ styleMarker (colorOf #gitModified)
  "&.git-line-removed:before" $ do
    let size = 6px
    position =: absolute
    left     =: -5px
    bottom   =: -size
    height   =: 0
    width    =: 0
    content  =: "\" \""
    border   =: [solid, transparent]
    borderLeftColor =: rgba 1 0.2 0.2 0.5
    borderWidth     =: size
    marginTop       =: -size
    pointerEvents   =: none


styleMinimap :: MonadThunk m => StyleT m ()
styleMinimap = "atom-text-editor atom-text-editor-minimap" $ do
    opacity =: 0.5
    transition =: [opacity, animSpeedOf #minimapHover]
    "&:hover" $ do
      opacity =: 1
      transition =: [opacity, animSpeedOf #minimapHover]

    -- Don't touch it. If you change 0.75 value the blending could break
    -- until chrome blending gets fixed.
    #minimapVisibleArea $ do
      boxShadow =: [0,0,0,10000px, setAlpha 0.75 bgColor]
      "&::after" $ backgroundColor =: transparent

    #minimapControls $ display =: none
