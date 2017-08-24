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





styleGitMarkers :: MonadThunk m => StyleT m ()
styleGitMarkers = "atom-text-editor .gutter .line-number" $ do
  let clr = setAlpha 0.6 $ colorOf #text
  opacity =: 0.25
  color   =: clr
  "&.cursor-line" $ do
    opacity =: 0.8!important
    color   =: clr
  "&.git-line-added" $ do
    borderLeft =: [2px, solid, transparent]
    color      =: "#b5bd68"
    opacity    =: 0.5

  "&.git-line-modified" $ do
    borderLeft =: [2px, solid, transparent]
    color      =: "#de935f"
    opacity    =: 0.5




root :: MonadThunk m => StyleT m ()
root = do
  styleSettings
  styleTabs
  stylePanels

  styleGitMarkers



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
