{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Render.Less where

import Prologue
import Language.CSS.Hss.Class


import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ratio      (numerator, denominator)

import Data.Functor.Foldable

----------------------
-- === Less AST === --
----------------------

data LessVal = LessVal
  { _isImportant :: Bool
  , _lessRawVal  :: LessRawVal
  } deriving (Show)

data LessRawVal
  = LessVar Text
  | LessNum Text
  | LessTxt Text
  | LessApp Text [LessRawVal]
  deriving (Show)

data LessDecl
  = LessDefDecl     Text LessVal
  | LessSectionDecl Text [LessDecl]
  deriving (Show)


toAST :: [ValueDecl] -> [LessDecl]
toAST = fmap convert

instance Convertible ValueDecl LessDecl where
  convert = \case
    DefDecl     (Def name val) -> LessDefDecl name (convert val)
    SectionDecl (Section s b)  -> LessSectionDecl (convert s) (convert <$> toList b)
    a                          -> error $ "TODO: " <> show a

instance Convertible (Fix Val) LessVal where
  convert (Fix (Val flags v)) = LessVal (Set.member FlagImportant flags) $ convert v

instance Convertible (Fix Val) LessRawVal where
  convert (Fix (Val _ v)) = convert v

instance Convertible (RawVal (Fix Val)) LessRawVal where
  convert = \case
    Var   a -> LessVar a
    Txt   a -> LessTxt a
    App t a -> LessApp t (convert <$> a)
    Num (Number us a) -> LessNum . convert $ case Map.assocs us of
      [] -> rawNum
      [(u,i)] -> if i == 1 then rawNum <> show u else error $ "Cannot generate non singleton unit value " <> show u <> "^" <> show i
      ls -> error "Cannot generate non singleton unit value " <> show ls
      where rawNum = if denominator a == 1 then show (numerator a) else show (round a :: Int)

instance Convertible Selector Text where
  convert = \case
    SimpleSelector a -> a
    SubSelector l r  -> convert l <> " > " <> convert r


----------------------------
-- === Pretty printer === --
----------------------------

class PrettyPrinter a where pretty :: a -> Text

instance PrettyPrinter [LessDecl] where
  pretty = intercalate "\n" . fmap pretty

instance PrettyPrinter LessDecl where
  pretty = \case
    LessDefDecl     t v -> t <> ": " <> pretty v <> ";"
    LessSectionDecl s d -> s <> " {\n" <> pretty d <> "\n}"

instance PrettyPrinter LessVal where
  pretty (LessVal imp v) = go $ pretty v where
    go = if imp then (<> " !important") else id

instance PrettyPrinter LessRawVal where
  pretty = \case
    LessVar   a -> a
    LessNum   a -> a
    LessTxt   a -> a
    LessApp t a -> t <> " " <> intercalate " " (pretty <$> a)

---------------------------
-- === Less renderer === --
---------------------------

data Less

instance Renderer Pretty Less where
  render = pretty . toAST
