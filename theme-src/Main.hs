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
import Luna.Studio.Theme.UI.Scrollbar
import Luna.Studio.Theme.UI.Global
import Luna.Studio.Theme.UI.Message
import Luna.Studio.Theme.UI.TreeView


styleUI :: MonadThunk m => StyleT m ()
styleUI = do
  globalStyles
  styleSettings
  styleTabs
  stylePanels
  styleEditor
  styleScrollbars
  styleMessages
  styleTreeView
  styleTerminal
  styleStatusBar

styleVars :: MonadThunk m => StyleT m ()
styleVars = do
  "@bgColor" =: bgColor
  -- globalStyles

styleTerminal = do
  #terminalView . #standard $ do
    backgroundColor =: transparent
    color           =: colorOf #text

styleStatusBar = do
  #statusBar $ do
    "status-bar-file"        $ display =: none
    "status-bar-cursor"      $ display =: none
    "status-bar-selection"   $ display =: none
    "status-bar-launch-mode" $ display =: none
    fontSize =: sizeOf #text
    "&, a" $ color =: secondary $ colorOf #text

main :: IO ()
main = do
  (uiDecls, varsDecls) <- flip evalStateT (mempty :: ThunkMap) $ do
    ui   <- joinStyleT styleUI
    vars <- joinStyleT styleVars
    -- print =<< getSortedThunks
    -- pprint =<< get @ThunkMap
    evalThunkPassManager $ do
      registerThunkPass funcEvaluator
      evalThunks
    -- pprint =<< get @ThunkMap
    (,) <$> mapM fixDecl (toList ui)
        <*> mapM fixDecl (toList vars)
  -- pprint uiDecls
  -- print =<< readMVar thunkMapRef

  -- pprint r
  let uiLess   = Doc.renderLineBlock $ Doc.render $ render @Pretty @Less uiDecls
      varsLess = Doc.renderLineBlock $ Doc.render $ render @Pretty @Less varsDecls
  putStrLn $ convert uiLess
  writeFile "/home/wdanilo/github/luna-dark-ui/styles/style.css"     $ convert uiLess
  writeFile "/home/wdanilo/github/luna-dark-ui/styles/variables.less" $ convert varsLess
