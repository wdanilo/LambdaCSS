{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Layered
import qualified Prelude as P
import Prologue hiding (none, (%=), assign)
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

data Font = Font { _size :: Expr }
makeLenses ''Font

instance Mempty    Font where mempty  = Font (12 px)
instance Semigroup Font where (<>)    = undefined -- FIXME
instance P.Monoid  Font where mempty  = mempty
                              mappend = (<>)

fontMap :: Map Text Font
fontMap = fromList
  [ (#base    , Font (12px))
  , (#section , Font (36px))
  ]

fontOf :: Text -> Font
fontOf t = fontMap ^. ix t

fontSizeOf :: Text -> Expr
fontSizeOf t = fontOf t ^. size

marginMap :: Map Text Expr
marginMap = fromList
  [ (#base  , base)
  , (#panel , base * 2)
  , (#item  , base)
  ]
  where base = 20px

radiusMap :: Map Text Expr
radiusMap = fromList
  [ (#button , 8px)
  ]

marginOf :: Text -> Expr
marginOf t = marginMap ^?! ix t

radiusOf :: Text -> Expr
radiusOf t = radiusMap ^?! ix t

uiSize :: Expr
uiSize = 14px
--
--
-- iconOffset = 0.4

-- iconStyle       :: Monad m => StyleT m ()
-- scaledIconStyle :: Monad m => Val -> StyleT m ()
-- iconStyle = scaledIconStyle 1.5
-- scaledIconStyle scale = do
--   let fss = round2 (fontSizeOf #base * scale)
--   "&::before" $ do
--     [ fontSize,
--       width,
--       height,
--       lineHeight] =: fss
--     top           =: 0
--     marginLeft    =: round2 (fss * iconOffset)
--     verticalAlign =: middle

-- setColor :: Style
-- setColor = do

menuItemOffset :: Expr
menuItemOffset = marginOf #item * 2 + (fontSizeOf #base)

root :: MonadThunk m => StyleT m ()
root = do
  position =: 2
  ".settings-view" $ do

    ------------------
    -- === Menu === --
    ------------------

    ".config-menu" $ do
      position =: relative
      marginLeft =: marginOf #panel
      minWidth   =: uiSize * 14 -- FIXME
      maxWidth   =: uiSize * 20 -- FIXME
      background =: none
      border     =: 0
      padding    =: 0

      ".nav > li" $ do
        borderRadius =: 0
        border       =: 0

        ".icon" $ do
          padding    =: 0
          fontSize   =: fontSizeOf #base
          marginLeft =: marginOf #item
          lineHeight =: menuItemOffset
          -- background =: none !important
          -- iconStyle





-- style :: Style
-- style = do
--   position =: relative
--   border   =: 0
--   padding  =: 0

-- test :: Free (ListCons Int) ()
-- test = do
--   freeCons 1
--   freeCons 1




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
  putStrLn $ convert $ render @Pretty @Less fdecls
