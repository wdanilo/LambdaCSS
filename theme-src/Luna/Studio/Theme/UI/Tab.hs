module Luna.Studio.Theme.UI.Tab where

import Prologue hiding (none, left)

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Luna.Studio.Theme.UI.Prim
import Luna.Studio.Theme.UI.Utils
import qualified GHC.Exts as Lits


iconPaddingTop  = (sizeOf #tab - iconSize) / 2
iconPaddingSide = 6px
iconSize        = 18px

styleTabs = do

  -- === Active tab highlight === --

  #tabBar . ".tab.active" $ do
    color      =: colorOf #text
    transition =: [color, animSpeedOf #tabSwitch]
    ".icon::before" $ do
      opacity    =: alpha $ colorOf #text
      transition =: [opacity, animSpeedOf #tabSwitch]



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
    color           =: modAlpha (* 0.4) (colorOf #text)
    transition      =: [color, animSpeedOf #tabSwitch]
    ".icon::before" $ do
      marginRight =: 6px !important
      position   =: inherit
      transition =: [opacity, animSpeedOf #tabSwitch]


    -- text
    #title $ do
      margin    =: [0, 10px] -- FIXME

    "&:not(.active) .title::before" $ do
      color =: modAlpha (*0.25) (colorOf #text)

    ".title:not(.icon-tools)::before" $ do
      display =: none

    -- icons
    #title $ do
      textAlign =: center
      fontSize  =: sizeOf #title
      "&::before" $ do
        marginRight =: 4px
        width       =: sizeOf #title
        height      =: sizeOf #title
        fontSize    =: sizeOf #title
        lineHeight  =: sizeOf #title


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
