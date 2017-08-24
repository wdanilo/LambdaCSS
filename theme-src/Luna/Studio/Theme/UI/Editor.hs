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


styleGitMarkers :: MonadThunk m => StyleT m ()
styleGitMarkers = "atom-text-editor .gutter .line-number" $ do
  let clr = setAlpha 0.2 $ colorOf #text
  let styleMarker c = do
        borderLeft =: [2px, solid, transparent]
        color      =: setAlpha 0.5 c
        "&.cursor-line" $ do
          color =: setAlpha 0.8 c

  opacity    =: 1
  color      =: clr
  marginLeft =: 10px

  "&.cursor-line" $ do
    color =: setAlpha 0.5 $ colorOf #text

  "&.git-line-added"    $ styleMarker "#b5bd68"
  "&.git-line-modified" $ styleMarker "#de935f"
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
