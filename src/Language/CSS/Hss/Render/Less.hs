{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Render.Less where

import Prologue
import Language.CSS.Hss.Class

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



----------------------
-- === Less AST === --
----------------------

data LessVal
  = LessVar Text
  | LessNum Text
  | LessTxt Text
  | LessApp Text [LessVal]
  deriving (Show)

data LessDecl
  = LessDefDecl     Text LessVal
  | LessSectionDecl Text [LessDecl]
  deriving (Show)


toAST :: [Decl] -> [LessDecl]
toAST = fmap convert

instance Convertible Decl LessDecl where
  convert = \case
    DefDecl     (Def name val) -> LessDefDecl name (convert val)
    SectionDecl (Section s b)  -> LessSectionDecl (convert s) (convert <$> toList b)
    a                          -> error $ "TODO: " <> show a

instance Convertible Val LessVal where
  convert = \case
    Var   a -> LessVar a
    Txt   a -> LessTxt a
    App t a -> LessApp t (convert <$> a)
    ValNum (Number us a) -> LessNum . convert $ case Map.assocs us of
      [] -> rawNum
      [(u,i)] -> if i == 1 then rawNum <> show u else error $ "Cannot generate non singleton unit value " <> show u <> "^" <> show i
      ls -> error "Cannot generate non singleton unit value " <> show ls
      where rawNum = if isIntegral a then show (round a) else show a

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