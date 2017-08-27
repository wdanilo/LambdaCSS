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


styleMessages = "atom-notifications atom-notification" $ do
  let styleLayer :: MonadThunk m => Text -> StyleT m ()
      styleLayer name = do
        convert ("&." <> name) $ do
          let bg = colorOf (name <> "Layer")
          "&.icon::before" $ backgroundColor =: bg
          #content         $ backgroundColor =: bg

  mapM styleLayer ([#error, #fatal, #warning, #info, #success] :: [Text])

  "&.error, &.fatal, &.warning, &.info, &.success" $ do
    "&::before" $ do
      paddingTop =: 12 px
      width =: 40 px !important
      borderRadius =: [radiusOf #message,0,0,radiusOf #message]
    ".btn.close-all" $ do
      zero [border]
      color           =: highlighted $ colorOf #text
      backgroundColor =: highlighted $ colorOf #layer
      "&:hover" $ do
        color           =: highlighted $ colorOf #text
        backgroundColor =: highlighted $ highlighted $ colorOf #layer
    ".close.icon" $ do
      color =: white
    #content $ do
      padding      =: 10px
      borderRadius =: [0,radiusOf #message,radiusOf #message,0]
      color        =: highlighted $ colorOf #text
      #message $ do
        zero [padding]
        marginBottom =: marginOf #description
        fontSize     =: sizeOf #title
        fontWeight   =: 800
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
