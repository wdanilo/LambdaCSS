module Luna.Studio.Theme.UI.TreeView where

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


styleTreeView :: MonadThunk m => StyleT m ()
styleTreeView = do
  -- We assume that tree view is on the left side
  -- There is no way to discover tree-view using available selectors
  ".atom-dock-inner.left .tab-bar" $ display =: none

  #treeView $ do
    overflowX =: hidden

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
    ".icon::before" $ opacity =: 0.9 * alpha (colorOf #text)
    "&:not(:hover)" $ do
      ".entry.directory .header, .entry.file .name" $ do
        opacity =: 0.25
    "&:hover" $ do
      ".entry.directory .header, .entry.file .name" $ do
        opacity =: 0.4
    ".entry.opened" $ "> .name, > .header" $ opacity =: 1 !important

    -- animation
    ".entry" $ do
      ".header, .header::before" $ transition =: [[color, animSpeedOf #treeView], [opacity, animSpeedOf #treeView]]
      ".name"                    $ transition =: [[color, animSpeedOf #treeView], [opacity, animSpeedOf #treeView]]
    ".name::before" $ do
      transition =: [color, animSpeedOf #treeView]
