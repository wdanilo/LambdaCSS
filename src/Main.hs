{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Main where

import Control.Monad.State.Layered
import qualified Prelude as P
import Prologue hiding (none, (%=), assign, round, left, right)
import Control.Monad.Free
-- import qualified Main2 as M2

import Language.CSS.Hss

import qualified Data.Map as Map
import           Data.Map (Map)

import Control.Concurrent.MVar
import System.IO.Unsafe
import Language.CSS.Hss.Value.Unit
import Language.CSS.Hss.Value.Number
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict (IntMap)
import Data.Color
-- import Data.Layout hiding (render)
import qualified Data.Layout as Doc


data Font = Font { _size :: Expr }
makeLenses ''Font

instance Mempty    Font where mempty  = Font (12px)
-- instance Semigroup Font where (<>)    = undefined -- FIXME
-- instance P.Monoid  Font where mempty  = mempty
--                               mappend = (<>)

fontMap :: Map Text Font
fontMap = fromList
  [ (#base       , Font (12px))
  , (#section    , Font (36px))
  , (#description, Font (13px))
  ]

fontOf :: HasCallStack => Text -> Font
fontOf t = fontMap ^?! ix t

fontSizeOf :: HasCallStack => Text -> Expr
fontSizeOf t = fontOf t ^. size

marginMap :: Map Text Expr
marginMap = fromList
  [ (#base               , base)
  , (#panel              , base * 2)
  , (#item               , base)
  , (#subSection         , uiSize * 5)
  , (#sectionSide        , uiSize * 4)
  , (#sectionDesc        , uiSize * 1.5)
  , (#sectionBody        , uiSize * 3)
  , (#description        , uiSize)
  , (#title              , uiSize)
  , (#secondaryInfo      , uiSize)
  , (#option             , uiSize * 1.5)
  , (#optionDescription  , uiSize / 2.5)
  ]
  where base = 20px

colorMap :: Map Text Expr
colorMap = fromList
  [ (#text  , rgba 1 1 1 0.6)
  , (#layer , rgba 1 1 1 0.05)
  ]

radiusMap :: Map Text Expr
radiusMap = fromList
  [ (#button , 8px)
  , (#card   , 40px)
  ]

marginOf :: HasCallStack => Text -> Expr
marginOf t = marginMap ^?! ix t

radiusOf :: HasCallStack => Text -> Expr
radiusOf t = radiusMap ^?! ix t

colorOf :: HasCallStack => Text -> Expr
colorOf t = colorMap ^?! ix t

uiSize :: Expr
uiSize = 12px
--
--
iconOffset :: Expr
iconOffset = 0.4

iconStyle       :: MonadThunk m => StyleT m ()
scaledIconStyleFor' :: MonadThunk m => Bool -> Expr -> Text -> StyleT m ()
iconStyle    = iconStyleFor #base
iconStyleFor = scaledIconStyleFor 1.5
iconStyleForHack = scaledIconStyleFor' True 1.5
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
menuItemOffset = marginOf #item * 2 + (fontSizeOf #base)

-- Warning: Less "fade" casts argument to percent
subtle :: Expr -> Expr
subtle a = "fade" a (100 * 0.4 * alpha a)

secondary :: Expr -> Expr
secondary a = "fade" a (100 * 0.5 * alpha a)

alpha :: Expr -> Expr
alpha a = "alpha" a

hover :: Expr -> Expr
hover a = "fadein" a (4pct)

selected :: Expr -> Expr
selected a = "fadein" a (8pct)

highlighted :: Expr -> Expr
highlighted a = "fadein" a (8pct)

disabled :: Expr -> Expr
disabled a = "fade" a (100 * 0.5 * alpha a)

darken :: Expr -> Expr -> Expr
darken = "darken"

lighten :: Expr -> Expr -> Expr
lighten = "lighten"

white = rgb 1 1 1
black = rgb 0 0 0


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


-- === Raw Less bindings === --
-- TODO: majority of the bindings should be removed and re-implemented here
accentColor        = var "accent-color"
accentBgLayerColor = var "accent-bg-layer-color"


-- c =: hover $ subtle $ colorOf #text
root :: MonadThunk m => StyleT m ()
root = do

  ------------------
  -- === Menu === --
  ------------------

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
          #icon $ setColor $ hover $ subtle $ colorOf #text
          backgroundColor =: hover (rgba 1 1 1 0)

        "&.active" . "&, &:hover" $ do
          #icon $ setColor $ colorOf #text
          background =: selected (rgba 1 1 1 0)

        #icon $ do
          padding    =: 0
          fontSize   =: fontSizeOf #base
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
          "&:hover" . setColor $ hover $ subtle $ colorOf #text
          setColor $ subtle $ colorOf #text
          iconStyle


  ----------------------
  -- === Sections === --
  ----------------------

  -- === Layout === --

  #settingsView $ do
    ".panels-item > .section:first-child" $ marginTop =: 0
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
      fontWeight =: 200
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
        #settingDescription $ margin =: [marginOf #optionDescription, 0]

    #settingDescription $ do
      color        =: secondary $ colorOf #text
      marginTop    =: 5px
      marginBottom =: 4px


  -------------------
  -- === Cards === --
  -------------------

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
      background    =: colorOf #layer
      marginBottom  =: 3px
      "&:hover"    $ background =: hover    $ colorOf #layer
      "&.disabled" $ background =: disabled $ colorOf #layer

      #packageDescription $ do
        display =: block
        margin  =: [marginOf #description, 0]

      #metaControls        $ do margin        =: 0
      #cardName            $ do marginBottom  =: marginOf #title
      #packageName         $ do color         =: colorOf  #text
      #packageVersion      $ do marginLeft    =: marginOf #secondaryInfo
                                color         =: subtle $ colorOf #text
      #metaUser            $ do display       =: none
      #statusIndicator     $ do display       =: none
      #packageDescription  $ do color         =: secondary $ colorOf #text
                                fontSize      =: fontSizeOf #description
      #stats . #value      $ do color         =: subtle $ colorOf #text
                                fontSize      =: fontSizeOf #description
      #stats . #icon       $ scaledIconStyleFor 1 #description
      #btnToolbar . #icon  $ do
        color =: secondary $ colorOf #text
        "&::before"        $ do color         =: secondary $ colorOf #text
                                marginRight   =: 7px
        "&:hover"          $ do color         =: colorOf #text
        "&.install-button" $ do btnVariant accentBgLayerColor

    #packageDetail $ do
      #section $ marginTop =: 0
      #packageContainer $ do
        marginBottom =: 20px
        ".package-card:hover" $ background =: colorOf #layer

      #breadcrumb $ do
        padding      =: 0
        marginLeft   =: marginOf #sectionSide
        marginTop    =: (marginOf #item - fontSizeOf #base) / 2
        marginBottom =: (marginOf #item - fontSizeOf #base) / 2


    ----------------------
    -- === Specific === --
    ----------------------

    -- === Top level seciton notes === --

    "[id=\"editor-settings-note\"], [id=\"core-settings-note\"], .native-key-bindings:not(.table)" $ do
      color =: secondary $ colorOf #text
      "&::before, .icon::before" $ display =: none


    -- === Key binding list === --

    ".native-key-bindings.table.text" $ do
      "table, th, td, tr"  $ do
        border =: none
        "&:first-child" $ borderRadius =: [20px, 0, 0, 20px]
        "&:last-child"  $ borderRadius =: [0, 20px, 20px, 0]
      "tr:nth-child(even)" $ backgroundColor =: disabled $ colorOf #layer


    -- === Theme chooser === --

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


-- modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b

main :: IO ()
main = do
  -- let c = rgb 1 2 3
  --     t = convert' c :: SomeTone '[RGB, HSL]
  -- print t
  -- print (convert t :: RGB)

  -- print $ foo <$> [0..10]

  -- M2.test
  -- pprint style
  fdecls <- flip evalStateT (mempty :: ThunkMap) $ do
    r <- joinStyleT root
    pprint r

    print =<< getSortedThunks
    pprint =<< get @ThunkMap
    evalThunkPassManager $ do
      registerThunkPass funcEvaluator
      evalThunks
    pprint =<< get @ThunkMap
    mapM fixDecl (toList r)

  pprint fdecls
  -- print =<< readMVar thunkMapRef

  -- pprint r
  return ()
  let css = Doc.renderLineBlock $ Doc.render $ render @Pretty @Less fdecls
  putStrLn $ convert css
  writeFile "/home/wdanilo/github/luna-dark-ui/styles/test.css" $ convert css
