{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Compile (
    JS(..)
  , NameCtxt(..)
  , compile
  , compile'
) where

import Prelude hiding (lookup)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Language.JStlc.Syntax

newtype JS = JS { toText :: T.Text }

data NameCtxt :: [Ty] -> * where
  CNil :: NameCtxt '[]
  (:>) :: T.Text -> NameCtxt as -> NameCtxt (a ': as)
infixr 5 :>

lookup :: Ix as a -> NameCtxt as -> T.Text
lookup IZ (x :> xs) = x
lookup (IS i) (_ :> xs) = lookup i xs

-- TODO: this machinery seems horrible, maybe we can get away without it

class Sing (a :: Ty) where
  sing :: STy a

instance Sing IntTy where
  sing = SIntTy

instance Sing BoolTy where
  sing = SBoolTy

instance Sing StringTy where
  sing = SStringTy

instance (Sing a, Sing b) => Sing (FnTy a b) where
  sing = SFnTy sing sing

instance Sing a => Sing (OptionTy a) where
  sing = SOptionTy sing

instance Sing a => Sing (ListTy a) where
  sing = SListTy sing

compile :: Sing a => Term '[] a -> JS
compile = compile' CNil

compile' :: Sing a => NameCtxt as -> Term as a -> JS
compile' c (Var i) = JS $ lookup i c
compile' _ (Lit v) = jsLit sing v
compile' c (Lam x _ body) = JS $
  "function (" <> T.pack x <> ") { return " <>
  toText (compile' ((T.pack x) :> c) body) <> "; }"
compile' c (App f x) = JS $ toText (compile' c f) <> "(" <>
  toText (compile' c x) <> ")"

jsLit :: STy a -> ValTy a -> JS
jsLit SIntTy n = JS $ T.pack $ show n
jsLit SBoolTy True = JS "true"
jsLit SBoolTy False = JS "false"
jsLit SStringTy (LitString s) = JS $ T.cons '"' $ flip T.snoc '"' $
  T.pack $ escape s
jsLit (SFnTy a b) _ = JS "<fn literal: this can't work>" -- TODO
jsLit (SOptionTy a) Nothing = JS "null"
jsLit (SOptionTy a) (Just x) = jsLit a x
jsLit (SListTy a) xs = JS $ T.cons '[' $ flip T.snoc ']' $ T.intercalate ", " $
  fmap (toText . jsLit a) xs

escape :: String -> String
escape = id -- TODO
