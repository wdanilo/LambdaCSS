{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Prologue as P
import Prologue hiding ((>))
import Control.Monad.Free


import Language.CSS.Hss



-- style :: SectionBody
-- style = ".config-menu" $ do
--   position := relative
--   border   := 0
--   padding  := 0
--
--   ".nav" > li $ do
--     border := 0

style :: SectionBody
style = do
  position := relative
  border   := 0
  padding  := 0

-- test :: Free (ListCons Int) ()
-- test = do
--   freeCons 1
--   freeCons 1


main :: IO ()
main = do
  pprint style
  putStrLn $ convert $ render @Pretty @Less $ toList style
