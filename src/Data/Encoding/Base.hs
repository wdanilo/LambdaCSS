module Data.Encoding.Base where

import Prelude
import Data.List
import Data.Maybe


---------------------------
-- === Base encoding === --
---------------------------

class IsBase base where
  charsOfBase :: [Char]

encode :: forall base. IsBase base => Int -> [Char]
encode n = reverse [chars !! (x `mod` len) | x <- takeWhile (> 0) (iterate (\x -> x `div` len) n)] where
  len   = length chars
  chars = charsOfBase @base

decode :: forall base. IsBase base => [Char] -> Int
decode code = foldl (\r c -> (len * r) + fromJust (elemIndex c chars)) 0 code where
  len   = length chars
  chars = charsOfBase @base


----------------------------------
-- === Base implementations === --
----------------------------------

data Base62
instance IsBase Base62 where
  charsOfBase = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

encode62 :: Int -> String
encode62 = encode @Base62

decode62 :: String -> Int
decode62 = decode @Base62
