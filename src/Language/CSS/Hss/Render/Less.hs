{-# LANGUAGE OverloadedStrings #-}

module Language.CSS.Hss.Render.Less where

import Prologue
import Language.CSS.Hss.Class



----------------------
-- === Less AST === --
----------------------

data LessVal
  = LessVar Text
  | LessNum Double
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
    Num   a -> LessNum a
    Txt   a -> LessTxt a
    App t a -> LessApp t (convert <$> a)

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
    LessNum   a -> convert $ show a
    LessTxt   a -> a
    LessApp t a -> t <> " " <> intercalate " " (pretty <$> a)


---------------------------
-- === Less renderer === --
---------------------------

data Less

instance Renderer Pretty Less where
  render = pretty . toAST
