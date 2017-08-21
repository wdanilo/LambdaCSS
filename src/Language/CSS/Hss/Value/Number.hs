{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.CSS.Hss.Value.Number where

import Prologue
import Language.CSS.Hss.Value.Unit

import qualified Data.Map.Strict as Map
import           Data.Map.Strict    (Map)


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

instance Num (Unit -> Number) where
  fromInteger i = flip Number (fromInteger i) . flip Map.singleton 1

instance Num Number where
  fromInteger = Number mempty . fromInteger
  -- (+)         = appM2 "+"
  -- (-)         = appM2 "-"
  -- (*)         = appM2 "*"
  -- abs         = appM1 "abs"
  -- signum      = appM1 "signum"

instance Show Number where
  showsPrec d (Number u r) = showParen' d
    $ showString "Number "
    . showsPrec' (toList u)
    . showString " "
    . showsPrec' (convertTo @Double r)
