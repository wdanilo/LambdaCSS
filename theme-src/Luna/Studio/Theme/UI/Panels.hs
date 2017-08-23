module Luna.Studio.Theme.UI.Panels where

import Prologue hiding (none)

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Luna.Studio.Theme.UI.Prim
import Luna.Studio.Theme.UI.Utils


stylePanels :: MonadThunk m => StyleT m ()
stylePanels = do
  addPanelBorder
  addPaneBorder


panelBorderStyle = [sizeOf #paneBorder, solid, colorOf #border]

addPanelBorder :: MonadThunk m => StyleT m ()
addPanelBorder = do
  "atom-panel" $ do
    position =: relative
    "&.top"    $ borderBottom =: panelBorderStyle
    "&.top"    $ borderBottom =: panelBorderStyle
    "&.left"   $ borderRight  =: panelBorderStyle
    "&.right"  $ borderLeft   =: panelBorderStyle
    "&.bottom" $ borderTop    =: panelBorderStyle
    "&.footer:last-child" $ borderBottom =: none
    "&.tool-panel:empty"  $ border       =: none


addPaneBorder :: MonadThunk m => StyleT m ()
addPaneBorder = do
  "atom-pane-container" $ do
    "atom-pane" $ do
      [ borderRight,
        borderBottom] =: panelBorderStyle
      position        =: relative;

      -- prevent atom-text-editor from leaking ouside might improve performance
      #itemViews $ overflow =: hidden

  "atom-workspace" $ do
    "& .panes" $ do
      nestedHideBorder_h
      nestedHideBorder_v
      "& > .vertical"   $ nestedHideBorder_h
      "& > .horizontal" $ nestedHideBorder_v

  where
    maxNesting = 10
    borderSel_h = ".horizontal > :last-child"
    borderSel_v = ".vertical > :last-child"

    hideBorder t = do
      t =: 0
      "& > .pane" $ t =: 0

    nestedHideBorderN i s f = "&" f >>$ when (i > 0) $
        subsection ("& > " <> s) $ nestedHideBorderN (pred i) s f

    hideBorder_h = hideBorder borderRight
    hideBorder_v = hideBorder borderBottom
    nestedHideBorder   = nestedHideBorderN maxNesting
    nestedHideBorder_h = nestedHideBorder borderSel_h hideBorder_h
    nestedHideBorder_v = nestedHideBorder borderSel_v hideBorder_v
