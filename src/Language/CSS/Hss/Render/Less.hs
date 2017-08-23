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
import           Data.Layout     hiding (render, indented)
import qualified Data.Layout     as Doc
import           Text.Printf

import Data.Functor.Foldable
import Language.CSS.Hss.Value.Number


showF :: IsString s => Rational -> s
showF = fromString . printf "%f" . convertTo @Double

smartShow :: IsString s => Rational -> s
smartShow i = if (denominator i == 1) then fromString (show $ numerator i) else showF i


indented t = hspacing 2 <> t

----------------------
-- === Less AST === --
----------------------

data LessVal
  = LessVar Text
  | LessNum Text
  | LessTxt Text
  | LessLst [LessVal]
  | LessApp Text [LessVal]
  | LessMod Text LessVal
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
  convert (Fix (ValueScheme v _)) = convert v

instance Convertible (RawValueScheme (Fix ValueScheme)) LessVal where
  convert = \case
    Var   a -> LessVar a
    Txt   a -> LessTxt a
    App t a -> LessApp t $ convert <$> a
    Lst   a -> LessLst   $ convert <$> a
    Num (Number us a) -> LessNum . convert $ case Map.assocs us of
      [] -> smartShow a
      [(u,i)] -> if | i == 1 && u == "px" -> rawNum <> convert u
                    | i == 1              -> smartShow a <> convert u
                    | otherwise           -> error $ "Cannot generate non singleton unit value " <> show u <> "^" <> show i
      ls -> error "Cannot generate non singleton unit value " <> show ls
      where rawNum = if denominator a == 1 then show (numerator a) else show (P.round a :: Int)
    Col c   -> LessTxt $ convert val  where
      col   = convert (c :: CSSColor) :: Color RGB
      body  = intercalate "," (show <$> [rnd $ col ^. r, rnd $ col ^. g, rnd $ col ^. b])
           <> "," <> smartShow (convert $ col ^. a)
      rnd c = P.round (c * 255) :: Int
      val   = "rgba(" <> body <> ")"
    Mod t a -> LessMod t $ convert a

instance Convertible Selector Text where
  convert = \case
    SimpleSelector a   -> a
    MergeSelector  l r -> convert l <> " "   <> convert r
    SubSelector    l r -> convert l <> " > " <> convert r


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
  pretty = \case
    LessVar   a -> "@" <> convert a
    LessNum   a -> convert a
    LessTxt   a -> if a == mempty then "\"\"" else convert a
    LessLst   a -> intercalate space $ pretty <$> a
    LessMod t a -> pretty a <+> "!" <> convert t
    LessApp t a -> if (Text.all (Char.isAlphaNum ||. (=='-')) t) && t /= "-"
      then convert t <> parensed (intercalate ", " (pretty <$> a))
      else parensed $ pretty (unsafeHead a) <+> convert t <+> intercalate space (pretty <$> unsafeTail a)


---------------------------
-- === Less renderer === --
---------------------------

data Less

instance Renderer Pretty Less where
  render = pretty . toAST
