module Main where

import Control.Monad.State.Layered
import qualified Prelude as P
import Prologue hiding (none, (%=), assign, round, left, right)
import Control.Monad.Free
-- import qualified Main2 as M2

import Language.CSS.Hss

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Control.Concurrent.MVar
import System.IO.Unsafe
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import qualified "containers" Data.IntMap.Strict as IntMap
import           "containers" Data.IntMap.Strict (IntMap)
import Data.Color
-- import Data.Layout hiding (render)
import qualified Data.Layout as Doc
import Data.Hashable (hash)

import Luna.Studio.Theme.UI.Prim
import Luna.Studio.Theme.UI.Utils

import Luna.Studio.Theme.UI.Panel
import Luna.Studio.Theme.UI.Tab
import Luna.Studio.Theme.UI.Settings
import Luna.Studio.Theme.UI.Editor


globalStyles = do
  "*"              $ boxSizing       =: borderBox
  "html"           $ fontSize        =: sizeOf #text
  "atom-workspace" $ backgroundColor =: bgColor



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
      boxShadow =: [0,0,0,1000px, setAlpha 0.75 bgColor]
      "&::after" $ backgroundColor =: transparent

    #minimapControls $ display =: none


-- | In order to show scrollbars only on hover, we need a special div structure,
--   which is rarely implemented among atom plugins.

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
      -- backgroundColor  =: colorOf #layer
      backgroundColor     =: inherit -- colorOf #layer
      backgroundClip =: contentBox

    -- "&::-webkit-scrollbar-thumb:hover" $ do
    --   borderRadius   =: 5px
    --   border         =: [8px, solid, transparent]
    --   background     =: colorOf #layer
    --   backgroundClip =: contentBox




    -- atom-text-editor
    --   ::-webkit-scrollbar-track
    --     background: @scrollbar-background-color-editor
    --
    --   ::-webkit-scrollbar-corner
    --     background: @scrollbar-background-color-editor
    --
    --   ::-webkit-scrollbar-thumb
    --     border-color: @scrollbar-background-color-editor
    --     background: @scrollbar-color-editor





root :: MonadThunk m => StyleT m ()
root = do
  globalStyles
  styleSettings
  styleTabs
  stylePanels
  styleEditor
  styleMinimap
  styleScrollbars



main :: IO ()
main = do
  fdecls <- flip evalStateT (mempty :: ThunkMap) $ do
    r <- joinStyleT root
    -- print =<< getSortedThunks
    -- pprint =<< get @ThunkMap
    evalThunkPassManager $ do
      registerThunkPass funcEvaluator
      evalThunks
    -- pprint =<< get @ThunkMap
    mapM fixDecl (toList r)

  -- pprint fdecls
  -- print =<< readMVar thunkMapRef

  -- pprint r
  let css = Doc.renderLineBlock $ Doc.render $ render @Pretty @Less fdecls
  putStrLn $ convert css
  writeFile "/home/wdanilo/github/luna-dark-ui/styles/style.css" $ convert css
