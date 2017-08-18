{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Prologue as P
import Prologue hiding ((>))
import Control.Monad.Free


import Language.CSS.Hss

import qualified Data.Map as Map
import           Data.Map (Map)




marginMap :: Map Text Number
marginMap = mempty
  & Map.insert #base  base
  & Map.insert #panel (base * 2)
  where base = 20

margin :: Convertible Number t => Text -> t
margin t = convert $ marginMap ^. ix t

uiSize = 14px

style :: SectionBody
style = ".config-menu" $ do
  position   := relative
  marginLeft := margin #panel
  border     := uiSize * 14
  padding    := 0

  ".nav" > li $ do
    border := 0

-- style :: SectionBody
-- style = do
--   position := relative
--   border   := 0
--   padding  := 0

-- test :: Free (ListCons Int) ()
-- test = do
--   freeCons 1
--   freeCons 1


main :: IO ()
main = do
  -- pprint style
  putStrLn $ convert $ render @Pretty @Less $ toList style
