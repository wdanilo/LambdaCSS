{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.CSS.Hss.Value.Number where

import Prologue
import Data.Map.Strict (Map)
import Language.CSS.Hss.Value.Unit


--------------------
-- === Number === --
--------------------

-- === Definition === --

data Number = Number
  { _numUnits :: Map Unit Int
  , _rawNum   :: Rational
  } deriving (Eq)
makeLenses ''Number


-- === Instances === --

instance Show Number where
  showsPrec d (Number u r) = showParen' d
    $ showString "Number "
    . showsPrec' (toList u)
    . showString " "
    . showsPrec' (convertTo @Double r)
