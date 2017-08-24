module Luna.Studio.Theme.UI.Panel where

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


---------------------------------------
-- === Panel recursive selectors === --
---------------------------------------

-- | Atom does not provide a simple way to select all panels that
--   are attached to the top or right border of the window, so we
--   have to create some ugly nested CSS to support it.

maxNesting = 10
lastSubH  = ".horizontal > :last-child"
lastSubV  = ".vertical > :last-child"
firstSubH = ".horizontal > :first-child"
firstSubV = ".vertical > :first-child"

withNestedPanelsRB' f = withNestedPanelsRB f f
withNestedPanelsRB fh fv = do
  "& > .vertical"   $ withNestedPanels lastSubH fh
  "& > .horizontal" $ withNestedPanels lastSubV fv

withNestedPanelsRT' f = withNestedPanelsRT f f
withNestedPanelsRT fh fv = do
  "& > .vertical"   $ withNestedPanels lastSubH  fh
  "& > .horizontal" $ withNestedPanels firstSubV fv

withNestedPanels = withNestedPanelsN maxNesting
withNestedPanelsN i s f = "&" f >>$ when (i > 0) $
    subsection ("& > " <> s) $ withNestedPanelsN (pred i) s f



--------------------------
-- === Panel styles === --
--------------------------

stylePanels :: MonadThunk m => StyleT m ()
stylePanels = do
  addPanelBorder
  addPaneBorder
  addPaneHighlight
  dimInactivePanels

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


-- === Borders between panels === --

hideBorder t = do
  t =: 0
  "& > .pane" $ t =: 0

hideBorder_h = hideBorder borderRight
hideBorder_v = hideBorder borderBottom

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
      withNestedPanels lastSubH hideBorder_h
      withNestedPanels lastSubV hideBorder_v
      withNestedPanelsRB hideBorder_h hideBorder_v


-- === Panel selection highlighting === --

addPaneHighlight = do
  let bcol = colorOf #panelHighlight
      bdr  = sizeOf  #paneBorder
      paneShadow        = Lits.fromList paneShadow'
      borderPaneShadow  = Lits.fromList borderPaneShadow'
      borderPaneShadow' = paneShadow' |> [bdr,0,0,0, bcol]
      paneShadow'       = [ [-bdr, -1px, 0, 0, bcol]
                          , [-bdr, -bdr, 0, 0, bcol] ]

      panelShadow        = Lits.fromList panelShadow'
      borderPanelShadow  = Lits.fromList borderPanelShadow'
      borderPanelShadow' = panelShadow' |> ["inset", 0, bdr, 0, 0, bcol]
      panelShadow'       = [ [bdr, -bdr, 0, 0, bcol] ]

  ".pane.active" $ do
    zIndex =: 10
    boxShadow =: paneShadow
    #insetPanel $ do
      zIndex     =: 100
      background =: bgColor
      boxShadow  =: panelShadow
    #itemViews $ boxShadow =: [ [-bdr, bdr, 0, 0, bcol]
                              , [0, bdr, 0, 0, bcol]
                              , [bdr, -bdr, 0, 0, bcol]
                              , [bdr, bdr, 0, 0, bcol]
                              ]

  "atom-workspace" $ do
    "& .panes" $ do
      let go = "&.pane.active, & > .pane.active" $ do
            #insetPanel $ boxShadow  =: borderPanelShadow
            -- boxShadow =: borderPaneShadow -- fixing small space right to the tab bar -- TODO: enable wisely, not to break nested borders
      withNestedPanelsRT' go
      return ()



-- === Inactive panels dim === --

dimInactivePanels = do
  "atom-workspace > .horizontal > .vertical" $ do
    #pane $ do
      opacity =: 0.8
      transition =: [opacity, animSpeedOf #paneDim]
    ".pane.active" $ do
      opacity =: 1
      transition =: [opacity, animSpeedOf #paneDim]
