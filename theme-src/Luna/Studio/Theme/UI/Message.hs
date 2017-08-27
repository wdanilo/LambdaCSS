module Luna.Studio.Theme.UI.Message where

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


styleMessages = "atom-notifications" $ do
  paddingRight =: 0
  "& > :first-child" $ do
    "&.icon::before" $ borderTopLeftRadius  =: radiusOf #message !important
    #content         $ borderTopRightRadius =: radiusOf #message !important
  "& > :last-child" $ do
    "&.icon::before" $ borderBottomLeftRadius  =: radiusOf #message !important
    #content         $ borderBottomRightRadius =: radiusOf #message !important

  "atom-notification" $ do
    let styleLayer :: MonadThunk m => Text -> StyleT m ()
        styleLayer name = do
          convert ("&." <> name) $ do
            let bg = colorOf (name <> "Layer")
            "&.icon::before" $ backgroundColor =: bg
            #content         $ backgroundColor =: bg

    mapM styleLayer ([#error, #fatal, #warning, #info, #success] :: [Text])

    #btn $ do
      border          =: 0                            !important
      color           =: highlighted (colorOf #text)  !important
      backgroundColor =: highlighted (colorOf #layer) !important
      "&:hover" $ do
        color           =: highlighted (colorOf #text)                !important
        backgroundColor =: highlighted (highlighted $ colorOf #layer) !important

    borderTop =: 0
    margin    =: 0

    "&.error, &.fatal, &.warning, &.info, &.success" $ do
      "&.icon::before" $ do
        paddingTop   =: 12 px
        width        =: 40 px !important
        borderRadius =: 0
        border       =: [3px, solid, bgColor]
        borderTop    =: 0

        borderRight =: 0

        color        =: colorOf #text
      ".close.icon" $ do
        opacity =: 1
        color   =: colorOf #text
        "&:hover" $ color =: hovered $ colorOf #text
      #content $ do
        padding      =: 10px
        borderRadius =: 0
        border       =: [3px, solid, bgColor]
        borderTop    =: 0
        color        =: highlighted $ colorOf #text
        #message $ do
          paddingLeft   =: 0
          paddingTop    =: 0
          paddingBottom =: 0
          marginBottom  =: marginOf #description
          fontSize      =: sizeOf #title
          fontWeight    =: 800
          "code" $ do
            backgroundColor =: transparent
            color           =: colorOf #text
        #detail $ do
          zero [padding, border, margin]
          fontSize        =: sizeOf #text
          backgroundColor =: transparent
          color           =: highlighted $ colorOf #text
          #stackToggle $ do
            marginTop =: marginOf #description
            color     =: highlighted $ colorOf #text
            ".icon::before" $ do
              fontSize    =: sizeOf #text
              marginRight =: 3px
          #stackContainer $ do
            zero [margin]
            marginLeft =: 20px
        #meta $ do
          zero [padding]
          color        =: highlighted $ colorOf #text
          marginTop    =: marginOf #description
          marginBottom =: marginOf #description
          border       =: none
      ".description a" $ fontStyle =: "italic"
      "a" $ color =: colorOf #text
