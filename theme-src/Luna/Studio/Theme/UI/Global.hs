module Luna.Studio.Theme.UI.Global where

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


globalStyles = do
  "*"              $ boxSizing       =: borderBox
  "html"           $ fontSize        =: sizeOf #text
  "atom-workspace" $ backgroundColor =: bgColor
