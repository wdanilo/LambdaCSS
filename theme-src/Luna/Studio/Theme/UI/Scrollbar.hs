module Luna.Studio.Theme.UI.Scrollbar where

import Prologue hiding (none)

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Luna.Studio.Theme.UI.Prim
import Luna.Studio.Theme.UI.Utils
import qualified GHC.Exts as Lits



-- | In order to show scrollbars only on hover, we need a special div structure,
--   which is rarely implemented among atom plugins. Thus we use two styles,
--   the proper one and a fallback.

styleScrollbars = do
  baseStyleScrollbars
  styleScrollbarsFallback
  styleProperScrollbars


baseStyleScrollbars = do
  "::-webkit-scrollbar" $ do
    width  =: 20px
    height =: 20px

  "::-webkit-scrollbar-corner" $ do
    background =: transparent


styleScrollbarsFallback = do
  ":hover" $ do
    "&::-webkit-scrollbar-thumb" $ do
      borderRadius    =: 20px
      border          =: [8px, solid, transparent]
      backgroundColor =: colorOf #layer
      backgroundClip  =: contentBox

  "&::-webkit-scrollbar-track" $ do
    background =: transparent

  "&::-webkit-scrollbar-thumb" $ do
    borderRadius   =: 20px
    border         =: [8px, solid, transparent]
    backgroundColor =: transparent
    backgroundClip  =: contentBox

  "&::-webkit-scrollbar-thumb:hover" $ do
    borderRadius   =: 20px
    border         =: [8px, solid, transparent]
    background     =: highlighted $ colorOf #layer
    backgroundClip =: contentBox


styleProperScrollbars = do
  ".vertical-scrollbar, .horizontal-scrollbar" $ do
    backgroundColor =: rgba 1 1 1 0
    transition =: [backgroundColor, 0.5s]

    "-webkit-background-clip" =: "text"
    "&:hover" $ do
      backgroundColor =: highlighted $ colorOf #layer
      transition =: [backgroundColor, 0.5s]

    "&::-webkit-scrollbar-track" $ do
      background =: transparent

    "&::-webkit-scrollbar-thumb" $ do
      borderRadius   =: 20px
      border         =: [8px, solid, transparent]
      backgroundColor =: inherit
      backgroundClip  =: contentBox
