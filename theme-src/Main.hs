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

--

zero :: MonadThunk m => [Pattern] -> StyleT m ()
zero = mapM_ (=: 0)

styleTreeView :: MonadThunk m => StyleT m ()
styleTreeView = do
  -- We assume that tree view is on the left side
  -- There is no way to discover tree-view using available selectors
  ".atom-dock-inner.left .tab-bar" $ display =: none

  #treeView $ do
    ".full-menu.list-tree" $ do
      marginLeft  =: 14px
      paddingLeft =: 6px
      minWidth =: 0
    ".selected::before" $ do
      backgroundColor =: colorOf #selectLayer
      borderRadius    =: 10px

    fontSize =: sizeOf #text
    #projectRoot $ do
      "& > .header" $ do
        fontSize =: sizeOf #title
        marginTop =: marginOf #title
        fontWeight =: 600 !important

    -- colors
    let entryColSel = "> .name, > .header .name, > .header .name::before, > .header::before"
    "&:not(:hover) .entry:not(.opened)" $ do
      ".name"                           $ color =: colorOf #text
      "&.status-modified" $ entryColSel $ color =: colorOf #text
      "&.status-added"    $ entryColSel $ color =: colorOf #text
      ".icon" $ "&::before" $ color =: colorOf #text
    ".entry" $ do
      ".name"                           $ color =: colorOf #text
      "&.status-modified" $ entryColSel $ color =: setAlpha (alpha $ colorOf #text) $ colorOf #gitModified
      "&.status-added"    $ entryColSel $ color =: setAlpha (alpha $ colorOf #text) $ colorOf #gitAdded
      ".default-icon, .icon-file-directory" $ "&::before" $ color =: white

    -- opacity
    ".icon::before" $ opacity =: alpha $ colorOf #text
    "&:not(:hover)" $ do
      ".entry.directory .header, .entry.file .name" $ do
        opacity =: 0.25
    "&:hover" $ do
      ".entry.directory .header, .entry.file .name" $ do
        opacity =: 0.4
    ".entry.opened" $ "> .name, > .header" $ opacity =: 1 !important

    -- animation
    ".entry" $ do
      ".header, .header::before" $ transition =: [[color, 0.2s], [opacity, 0.2s]]
      ".name"                    $ transition =: [[color, 0.2s], [opacity, 0.2s]]
    ".name::before" $ do
      transition =: [color, 0.2s]


  return ()

    --
    -- .selected
    --   color: @text-color
    --
    -- &:hover
    --   .file.list-item:not(.opened),
    --   .directory:not(.opened) > div
    --       opacity:0.5
    --       transition: opacity 0.2s
    --       &:hover
    --         opacity:0.7
    --         transition: opacity 0.2s







styleMessages = "atom-notifications atom-notification" $ do
  let styleLayer :: MonadThunk m => Text -> StyleT m ()
      styleLayer name = do
        convert ("&." <> name) $ do
          let bg = colorOf (name <> "Layer")
          "&.icon::before" $ backgroundColor =: bg
          #content         $ backgroundColor =: bg

  mapM styleLayer ([#error, #fatal, #warning, #info, #success] :: [Text])

  "&.error, &.fatal, &.warning, &.info, &.success" $ do
    "&::before" $ do
      paddingTop =: 12 px
      width =: 40 px !important
      borderRadius =: [radiusOf #message,0,0,radiusOf #message]
    ".btn.close-all" $ do
      zero [border]
      color           =: highlighted $ colorOf #text
      backgroundColor =: highlighted $ colorOf #layer
      "&:hover" $ do
        color           =: highlighted $ colorOf #text
        backgroundColor =: highlighted $ highlighted $ colorOf #layer
    ".close.icon" $ do
      color =: white
    #content $ do
      padding      =: 10px
      borderRadius =: [0,radiusOf #message,radiusOf #message,0]
      color        =: highlighted $ colorOf #text
      #message $ do
        zero [padding]
        marginBottom =: marginOf #description
        fontSize     =: sizeOf #title
        fontWeight   =: 800
      #detail $ do
        zero [padding, border, margin]
        fontSize        =: sizeOf #text
        backgroundColor =: transparent
        color           =: highlighted $ colorOf #text
        #stackToggle $ do
          marginTop =: marginOf #description
          color     =: highlighted $ colorOf #text
          ".icon::before" $ do
            fontSize    =: sizeOf #text
            marginRight =: 3px
        #stackContainer $ do
          zero [margin]
          marginLeft =: 20px
      #meta $ do
        zero [padding]
        color        =: highlighted $ colorOf #text
        marginTop    =: marginOf #description
        marginBottom =: marginOf #description
        border       =: none


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
      boxShadow =: [0,0,0,10000px, setAlpha 0.75 bgColor]
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
  styleMessages
  styleTreeView



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
