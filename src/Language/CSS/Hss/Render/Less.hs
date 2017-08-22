{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Render.Less where

import Prologue as P hiding (nested)
import Language.CSS.Hss.Class


import           "containers" Data.Set        (Set)
import qualified "containers" Data.Set        as Set
import           "containers" Data.Map.Strict (Map)
import qualified "containers" Data.Map.Strict as Map
import           Data.Ratio      (numerator, denominator)
import           Data.Color
import qualified Data.Char       as Char
import qualified Data.Text       as Text
import           Data.Layout     hiding (render)
import           Text.Printf

import Data.Functor.Foldable
import Language.CSS.Hss.Value.Number


showF :: IsString s => Double -> s
showF = fromString . printf "%f"

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
  | LessLst [LessRawVal]
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

instance Convertible (Fix ValueScheme) LessVal where
  convert (Fix (ValueScheme flags v _)) = LessVal (Set.member "important" flags) $ convert v

instance Convertible (Fix ValueScheme) LessRawVal where
  convert (Fix (ValueScheme _ v _)) = convert v

instance Convertible (RawValueScheme (Fix ValueScheme)) LessRawVal where
  convert = \case
    Var   a -> LessVar a
    Txt   a -> LessTxt a
    App t a -> LessApp t $ convert <$> a
    Lst   a -> LessLst   $ convert <$> a
    Num (Number us a) -> LessNum . convert $ case Map.assocs us of
      [] -> rawNum
      [(u,i)] -> if i == 1 then rawNum <> convert u else error $ "Cannot generate non singleton unit value " <> show u <> "^" <> show i
      ls -> error "Cannot generate non singleton unit value " <> show ls
      where rawNum = if denominator a == 1 then show (numerator a) else show (P.round a :: Int)
    Col c   -> LessTxt $ convert val  where
      col   = convert (c :: CSSColor) :: Color RGB
      body  = intercalate "," (show <$> [rnd $ col ^. r, rnd $ col ^. g, rnd $ col ^. b])
           <> "," <> showF (col ^. a)
      rnd c = P.round (c * 255) :: Int
      val   = "rgba(" <> body <> ")"

instance Convertible Selector Text where
  convert = \case
    SimpleSelector a -> a
    SubSelector l r  -> convert l <> " > " <> convert r


----------------------------
-- === Pretty printer === --
----------------------------

class PrettyPrinter a where pretty :: a -> Doc Text

instance PrettyPrinter [LessDecl] where
  pretty = foldr (</>) mempty . fmap pretty

instance PrettyPrinter LessDecl where
  pretty = \case
    LessDefDecl     t v -> convert t <> ":" <+> pretty v <> ";"
    LessSectionDecl s d -> convert s <+> "{" </> (indented . block $ pretty d) </> "}"

instance PrettyPrinter LessVal where
  pretty (LessVal imp v) = go $ pretty v where
    go = if imp then (<> " !important") else id

instance PrettyPrinter LessRawVal where
  pretty = \case
    LessVar   a -> "@" <> convert a
    LessNum   a -> convert a
    LessTxt   a -> convert a
    LessLst   a -> intercalate space $ parensed . pretty <$> a
    LessApp t a -> if Text.all Char.isAlphaNum t
      then convert t <> parensed (intercalate ", " (pretty <$> a))
      else pretty (unsafeHead a) <+> convert t <+> intercalate space (pretty <$> unsafeTail a)


---------------------------
-- === Less renderer === --
---------------------------

data Less

instance Renderer Pretty Less where
  render = pretty . toAST
