module Luna.Studio.Theme.UI.Settings where

import Prologue hiding (none, (%=), assign, round, left, right)

import Language.CSS.Hss
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import Data.Color

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

import Luna.Studio.Theme.UI.Prim
import Luna.Studio.Theme.UI.Utils


iconOffset :: Expr
iconOffset = 0.4

iconStyle           :: MonadThunk m => StyleT m ()
scaledIconStyleFor' :: MonadThunk m => Bool -> Expr -> Text -> StyleT m ()
iconStyle          = iconStyleFor #base
iconStyleFor       = scaledIconStyleFor 1.5
iconStyleForHack   = scaledIconStyleFor' True 1.5
scaledIconStyleFor = scaledIconStyleFor' False
scaledIconStyleFor' useHack scale s = do
  let fss = round (fontSizeOf s * scale)
  when useHack $ lineHeight =: fss
  "&::before" $ do
    [ fontSize,
      width,
      height,
      lineHeight] =: fss
    top           =: 0
    marginRight   =: round (fss * iconOffset)
    verticalAlign =: if useHack then bottom else middle

setColor :: MonadThunk m => Expr -> StyleT m ()
setColor c = do
  color =: c
  "&::before" $ color =: c

setSectionColor c = do
  color =: c;
  "&::before" $ color =: "fadeout"  c (10pct)

menuItemOffset :: Expr
menuItemOffset = marginOf #item * 2 + (sizeOf #text)


focusMixin :: MonadThunk m => StyleT m ()
focusMixin = do
  outline     =: none
  borderColor =: accentColor
  boxShadow   =: [0,0,0,1px, accentColor]



btnDefault :: MonadThunk m => Expr -> Expr -> Expr -> Expr -> StyleT m ()
btnDefault baseColor hoverColor selectedColor textColor = do
  color             =: textColor
  textShadow        =: none
  backgroundColor   =: baseColor
  "&:hover" $ color =: highlighted $ colorOf #text
  "&:active"   $ do
    background      =: darken baseColor (4pct)
    boxShadow       =: none
  "&.selected" $ do
    background      =: selectedColor;
  "&.selected:focus, &.selected:hover" $ do
    background      =: lighten selectedColor (2pct)
  "&:focus" focusMixin


btnVariant :: MonadThunk m => Expr -> StyleT m ()
btnVariant baseColor = do
  let ncolor = "contrast" baseColor white black (15pct)
  btnDefault baseColor
             (lighten baseColor (3pct))
             ("saturate" (darken baseColor (12pct)) (20pct))
             (highlighted $ colorOf #text)

  color =: ncolor;

  "&:hover, &:focus" $ color =: ncolor
  "&:focus" $ do
    borderColor    =: transparent
    backgroundClip =: paddingBox
    -- boxShadow: inset 0 0 0 1px fade(@base-border-color, 50%), 0 0 0 1px @color;

  "&.icon:before" $ color =: ncolor;



configMenuStyle :: MonadThunk m => StyleT m ()
configMenuStyle = do
  #settingsView $ do
    #configMenu $ do
      position   =: relative
      marginLeft =: marginOf #panel
      minWidth   =: uiSize * 14 -- FIXME
      maxWidth   =: uiSize * 20 -- FIXME
      background =: none
      border     =: 0
      padding    =: 0

      ".nav > li" $ do
        borderRadius =: radiusOf #button
        "&:hover" $ do
          #icon $ setColor $ hovered $ subtle $ colorOf #text
          backgroundColor =: colorOf #hoverLayer

        "&.active" . "&, &:hover" $ do
          #icon $ setColor $ colorOf #text
          background =: colorOf #selectLayer

        #icon $ do
          padding    =: 0
          fontSize   =: sizeOf #title
          marginLeft =: marginOf #item
          lineHeight =: menuItemOffset
          background =: none !important
          setColor . subtle $ colorOf #text
          iconStyle

      #buttonArea $ do
        margin =: 0

        #btn $ do
          whiteSpace =: initial
          textAlign  =: left
          padding    =: 0
          marginLeft =: marginOf #item
          lineHeight =: menuItemOffset
          background =: none
          "&:hover" . setColor $ hovered $ subtle $ colorOf #text
          setColor $ subtle $ colorOf #text
          iconStyle


sectionStyle :: MonadThunk m => StyleT m ()
sectionStyle = do

  -- === Layout === --
  #settingsView $ do
    ".panels-item > .section:first-child" $ marginTop =: 5px
    #subSection $ marginTop =: marginOf #subSection
    #section $ do
      padding =: 0 !important
      margin  =: marginOf #sectionSide
      #sectionContainer $
        maxWidth =: 600px;
      "section-heading, .sub-section-heading" $ do
        marginBottom =: marginOf #sectionDesc
      #sectionBody $
        marginTop =: marginOf #sectionBody;

  -- === Look === --
  #settingsView $ do
    #panels  . #section $ "&, &.settings-panel" $ border =: 0 !important
    #section . #icon    $ "&.section-heading, &.sub-section-heading" $ do
      setSectionColor $ colorOf #text
      fontSize   =: fontSizeOf #section
      fontWeight =: 300 !important
      iconStyleForHack #section
      #badge $ do
        marginLeft =: 10px
        fontSize   =: 15px
        color      =: secondary $ colorOf #text
        background =: colorOf #layer;

  -- === Controls layout === --
  #settingsView $ do
    #controlGroup $ do
      marginTop    =: marginOf #option !important
      marginBottom =: marginOf #option !important
      #checkbox $ do
        margin =: 0;
        #settingDescription $ margin =: [marginOf #description, 0]

    #settingTitle $ do
      color    =: colorOf #text
      fontSize =: sizeOf  #title
    #settingDescription $ do
      color        =: secondary $ colorOf #text
      fontSize     =: sizeOf #description
      marginTop    =: 5px
      marginBottom =: 4px


cardStyle :: MonadThunk m => StyleT m ()
cardStyle = do
  #settingsView $ do

    #packageContainer $ do
      borderRadius    =: radiusOf #card
      overflow        =: hidden
      ":last-child .package-card" $ do
        margin =: 0 !important

    #packageCard $ do
      border        =: 0
      borderRadius  =: 0
      padding       =: 19px
      background    =: colorOf #bglayer
      marginBottom  =: 3px
      fontSize      =: sizeOf #text
      "&:hover"    $ background =: hovered  $ colorOf #bglayer
      "&.disabled" $ background =: disabled $ colorOf #bglayer

      #packageDescription $ do
        display =: block
        margin  =: [marginOf #packageDescription, 0]

      #metaControls        $ do margin        =: 0
      #cardName            $ do marginBottom  =: marginOf #title
                                fontSize      =: sizeOf #title
      #packageName         $ do color         =: colorOf  #text
      #packageVersion      $ do marginLeft    =: marginOf #secondaryInfo
                                color         =: subtle $ colorOf #text
                                fontSize      =: sizeOf #text
                                fontWeight    =: normal
      #metaUser            $ do display       =: none
      #statusIndicator     $ do display       =: none
      #packageDescription  $ do color         =: secondary $ colorOf #text
      #stats . #value      $ do color         =: subtle $ colorOf #text
      #stats . #icon       $ do scaledIconStyleFor 1 #description
                                lineHeight    =: normal
      #stats . #statsItem  $ do lineHeight    =: normal
      #btnToolbar . #icon  $ do
        fontSize =: sizeOf #title
        color =: secondary $ colorOf #text
        "&::before"        $ do color         =: secondary $ colorOf #text
                                marginRight   =: 7px
        "&:hover"          $ do color         =: colorOf #text
                                "&::before" $ color =:colorOf #text
        "&.install-button" $ do btnVariant accentBgLayerColor

    #packageDetail $ do
      #section $ marginTop =: 0
      #packageContainer $ do
        marginBottom =: 20px
        ".package-card:hover" $ background =: colorOf #layer

      #breadcrumb $ do
        padding      =: 0
        marginLeft   =: marginOf #sectionSide
        marginTop    =: (marginOf #item - sizeOf #text) / 2
        marginBottom =: (marginOf #item - sizeOf #text) / 2

    -- button roundness
    #meta . #btnToolbar . #btnGroup $ do
      ".btn.enablement" $ borderRadius =: 0 (1em) (1em) 0
      "[style=\"display: none;\"] + .btn.enablement" $ do
        borderRadius =: 1em


specificSettingsStyle :: MonadThunk m => StyleT m ()
specificSettingsStyle = do
  -- === Top level seciton notes === --
  #settingsView $ do
    "[id=\"editor-settings-note\"], [id=\"core-settings-note\"], .native-key-bindings:not(.table)" $ do
      color =: secondary $ colorOf #text
      "&::before, .icon::before" $ display =: none


  -- === Key binding list === --
  #settingsView $ do
    ".native-key-bindings.table.text" $ do
      "table, th, td, tr"  $ do
        border =: none
        "&:first-child" $ borderRadius =: [20px, 0, 0, 20px]
        "&:last-child"  $ borderRadius =: [0, 20px, 20px, 0]
      "tr:nth-child(even)" $ backgroundColor =: disabled $ colorOf #layer


  -- === Theme chooser === --
  #settingsView $ do
    #themesPanel . #themesPicker $ do
      marginTop =: marginOf #sectionBody
      #controlGroup $ do
        lineHeight =: 100pct
        margin     =: (mkTxtExpr "1.5px") !important -- FIXME [WD]: why setting 2px margin here do not overlap while 1px does?
        #settingDescription $ display =: none
        #controls . #controlLabel $ do
          display =: inlineBlock
          float   =: left
          width   =: 150px
          #text $ do
            fontWeight =: normal
            margin     =: 0
            marginTop  =: 10px

        #controls . #selectContainer $ do
          ":first-child" $ do
            borderRadius =: 0
          ":last-child" $ do
            background   =: colorOf #layer
            borderRadius =: 0
            marginLeft   =: 0

        "&:first-child" . #controls . #selectContainer $ do
          ":first-child" $ borderTopLeftRadius  =: 12px
          ":last-child"  $ borderTopRightRadius =: 12px

        "&:last-child" . #controls . #selectContainer $ do
          ":first-child" $ borderBottomLeftRadius  =: 12px
          ":last-child"  $ borderBottomRightRadius =: 12px


  -- === Updates view === --
  #settingsView $ do
    #updatesBtnGroup $ do
      #btn $ do
        fontSize     =: 14px
        borderRadius =: 0
      "& :first-child" $ do
        margin                  =: 0
        borderTopLeftRadius     =: 1em
        borderBottomLeftRadius  =: 1em
      "& :last-child" $ do
        marginLeft              =: 1px
        borderTopRightRadius    =: 1em
        borderBottomRightRadius =: 1em

      -- If no internet connection, update button is hidden:
      "& [style=\"display: none;\"] + .btn" $ do
        borderRadius =: 1em


componentStyle :: MonadThunk m => StyleT m ()
componentStyle = do
  -- === Links === --
  #settingsView . ".link, .link:hover" $ do
    color =: accentColorSubtle


  -- === Mini editor === --
  "atom-text-editor[mini]" $ do
    overflow        =: auto
    fontSize        =: sizeOf #text
    lineHeight      =: sizeOf #line
    maxHeight       =: sizeOf #line * 5 -- rows
    paddingLeft     =: marginOf #input
    borderRadius    =: 100px -- FIXME
    color           =: colorOf #text
    backgroundColor =: colorOf #layer
    #placeholderText $ do
      color =: subtle $ colorOf #text


  -- === Drop-down lists === --
  #formControl $ do
    "option" $ backgroundColor =: bakedColorOf #layer !important
    border        =: 0
    fontSize      =: sizeOf #text * 1.25
    paddingTop    =: 0
    paddingBottom =: 0
    borderRadius  =: 20px
    "&, &:active, &:hover" $ do
      background =: colorOf #layer !important


  -- === Loading boxes === --
  #settingsView . #alert $ do
    padding      =: [1em, 1em, 1em, 1em]
    borderRadius =: 1em
    background   =: colorOf #layer
    "&, &::before" $ color =: colorOf #text

    -- beginning of errors (like pkg fetching error) use this class ...
    #nativeKeyBindings $ color  =: colorOf #text
    #errorLink         $ color  =: colorOf #text
    #padded            $ margin =: 0 (-2em) (-2em) (-2em)

    #errorDetails $ do
      borderRadius            =: 0
      borderBottomLeftRadius  =: 1em
      borderBottomRightRadius =: 1em


  -- === Checkbox === --
  let switchWidth  = 34px
      switchHeight = 16px
      toggleOffset = 4px
      toggleSize   = switchHeight - toggleOffset * 2
      toggleMargin = (switchHeight - toggleSize) / 2
  #settingsView . #checkbox $ do
    let togglePadding = switchWidth + marginOf #inlineControl
    paddingLeft =: togglePadding
    "label" $ vcenterChildren
    #inputCheckbox $ do
      appearance        =: none
      display           =: inlineBlock
      fontSize          =: inherit
      margin            =: [0, 0, 0, -togglePadding]
      width             =: switchWidth
      height            =: switchHeight
      cursor            =: pointer
      borderRadius      =: roundnessOf #switch * switchHeight / 2
      backgroundColor   =: colorOf #layer
      transition        =: backgroundColor "0.2s" $ "cubic-bezier" 0.5 0.15 0.2 1

      "&:before" $ do
        content         =: ""
        boxSizing       =: borderBox
        display         =: inlineBlock
        left            =: toggleMargin
        top             =: toggleMargin
        margin          =: 0
        width           =: toggleSize
        height          =: toggleSize
        borderRadius    =: inherit
        backgroundClip  =: contentBox
        backgroundColor =: colorOf #toggle -- rgb 1 0 0 -- @base-background-color
        transition      =: transform "0.2s" $ "cubic-bezier" 0.5 0.15 0.2 1

      "&:active, &:checked" $ do
        backgroundColor =: colorOf #layer

      "&:checked:before" $ do
        transform =: "translateX" (switchWidth - toggleSize - 2 * toggleMargin)

      "&:before" $ do
        opacity   =: 1
        transform =: none

      "&:after" $ do
        content =: none


  -- === Slider === --
  #settingsView . #slider $ do
    marginTop      =: 10px
    marginBottom   =: 10px
    #label $ color =: secondary (colorOf #text) !important
    #inputRange $ do
      zIndex       =: 0
      margin       =: 0
      position     =: relative
      appearance   =: none
      background   =: transparent
      display      =: inlineBlock
      height       =: switchHeight -- FIXME
      borderRadius =: roundnessOf #switch * switchHeight / 2
      overflow     =: hidden

      "&, &:focus" $ do
        "&::-webkit-slider-thumb" $ do
          height       =: switchHeight;
          width        =: 0
          borderRadius =: 0
          cursor       =: pointer
          boxShadow    =: [-400px, 0, 0 ,400px, colorOf #layer]

      "&::-webkit-slider-runnable-track" $ do
        width        =: 100pct
        height       =: switchHeight
        cursor       =: pointer
        background   =: colorOf #layer
        borderRadius =: roundnessOf #switch * switchHeight / 2


styleSettings :: MonadThunk m => StyleT m ()
styleSettings = do
  configMenuStyle
  sectionStyle
  cardStyle
  specificSettingsStyle
  componentStyle
