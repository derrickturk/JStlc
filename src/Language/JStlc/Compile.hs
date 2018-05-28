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

import Language.JStlc.JS
import Language.JStlc.Syntax

data NameCtxt :: [Ty] -> * where
  CNil :: NameCtxt '[]
  (:>) :: T.Text -> NameCtxt as -> NameCtxt (a ': as)
infixr 5 :>

lookup :: Ix as a -> NameCtxt as -> T.Text
lookup IZ (x :> xs) = x
lookup (IS i) (_ :> xs) = lookup i xs

compile :: Term '[] a -> JS
compile = compile' CNil

compile' :: NameCtxt as -> Term as a -> JS
compile' c (Var i) = JSVar $ lookup i c
compile' _ (Lit v) = toJS v
compile' c (Lam x _ body) =
  JSLambda (T.pack x) (compile' (T.pack x :> c) body)
compile' c (App f x) = JSCall (compile' c f) [(compile' c x)]
compile' c None = JSVar "null"
compile' c (Some t) = compile' c t
compile' c Nil = JSArray []
compile' c (Cons x Nil) = JSArray [compile' c x]
compile' c (Cons x xs) =
  JSMethod (JSArray [compile' c x]) "concat" [(compile' c xs)]
