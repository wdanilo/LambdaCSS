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

import Luna.Studio.Theme.UI.Panels
import Luna.Studio.Theme.UI.Settings



iconPaddingTop  = (sizeOf #tab - iconSize) / 2
iconPaddingSide = 6px
iconSize        = 18px

styleTabs = do

  ".pane.active" $ do
    zIndex =: 10
    -- border =: 10 px
    -- boxShadow =: "-3px 0 0 0 #660000, -3px -3px 0 0 #660000"
    boxShadow =: "-3px -1px 0px 0px #660000, -3px -3px 0px 0px #660000"
    #insetPanel $ do
      zIndex     =: 1
      background =: bgColor
      boxShadow  =: "3px -3px 0 0 #660000"
    #itemViews  $ boxShadow =: "-3px 3px 0 0px #660000, 0px 3px 0 0px #660000, 3px -4px 0 0px #660000, 3px 3px 0 0px #660000"


  -- === Active tab highlight === --

  #tabBar . ".tab.active" $ do
    color =: colorOf #text
    ".icon::before" $ opacity =: alpha $ colorOf #text


  -- === Padding === --
  #tabBar $ do
    position     =: relative
    height       =: sizeOf #tab + marginOf #tab
    background   =: none
    overflowX    =: auto
    overflowY    =: hidden
    borderRadius =: 0
    "&::-webkit-scrollbar" $ display =: none
    "&:empty"              $ display =: none

    -- keep tabs the same size when activated
    ".tab, .tab.active" $ do
      paddingRight     =: 0
      #title $ padding =: 0


  -- === Labels === --

  #tabBar . #tab $ do
    position        =: relative
    top             =: 0
    padding         =: 0
    margin          =: 0
    height          =: inherit
    fontSize        =: inherit
    lineHeight      =: sizeOf #tab
    backgroundColor =: none
    color           =: inactive (colorOf #text)
    ".icon::before" $ do
      position =: inherit
      opacity  =: alpha $ inactive (colorOf #text)

    -- text
    #title $ do
      margin    =: [0, 10px] -- FIXME

    -- icons
    ".title.title:before" $ do
      marginRight =: 4px
      width       =: sizeOf #text
      height      =: sizeOf #text
      fontSize    =: sizeOf #text
      lineHeight  =: sizeOf #text


  -- === Close icon === --

  #tabBar . #tab $ do
    #closeIcon $ do
      top                =: iconPaddingTop
      left               =: iconPaddingSide
      lineHeight         =: iconSize
      fontSize           =: sizeOf #text
      zIndex             =: 2
      width              =: iconSize
      height             =: iconSize
      borderRadius       =: 10px
      backgroundColor    =: inherit
      overflow           =: hidden
      transform          =: scale 0
      color              =: inactive $ colorOf #text
      transitionDuration =: animSpeedOf #tabCloseIcon
      background         =: bgColor

      "&::before" $ do
        color =: setAlpha ctxColorAlpha accentColor

      "&:hover::before" $ do
        color =: accentColor

      "&::before" $ do
        zIndex        =: 1
        position      =: absolute
        fontSize      =: sizeOf #text
        width         =: inherit
        height        =: inherit
        lineHeight    =: inherit
        textAlign     =: center
        pointerEvents =: none

    "&:hover .close-icon" $ do
      transform          =: scale 1
      transitionDuration =: animSpeedOf #tabCloseIcon


  #tabBar . ".tab.modified" $ do
    "&:hover .close-icon" $ do
      color =: accentColor

    "&:not(:hover) .close-icon" $ do
      top          =: iconPaddingTop
      left         =: iconPaddingSide
      width        =: iconSize
      height       =: iconSize
      lineHeight   =: iconSize
      color        =: accentColorWide
      border       =: none
      transform    =: scale 1
      "&::before" $ do
        content =: "\"\\f052\"" -- FIXME: add String CSS type
        display =: inlineBlock


  -- === Context === --

  let ctxColor = setAlpha 0.4 accentColor

  #tabBar $ do
    ".tab::before" $ do
      display     =: none -- FIXME: Currently disabled, because we do not have any contexts!
      content     =: ""
      position    =: absolute
      zIndex      =: 2
      marginRight =: 50px
      width       =: 100pct
      height      =: 3px
      background  =: modAlpha (* 0.5) ctxColor

    ".tab.active:before" $
      background  =: ctxColor

    ".tab:not(:last-child)::before" $ do
      width =: 100pct - 2px






root :: MonadThunk m => StyleT m ()
root = do
  styleSettings
  stylePanels

  styleTabs


main :: IO ()
main = do
  fdecls <- flip evalStateT (mempty :: ThunkMap) $ do
    r <- joinStyleT root
    -- pprint r
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
