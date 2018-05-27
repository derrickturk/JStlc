{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.JStlc.Eval (
    Ctxt(..)
  , eval
) where

import Prelude hiding (lookup)

import Language.JStlc.Syntax

data Ctxt :: [Ty] -> * where
  CNil :: Ctxt '[]
  (:>) :: ValTy a -> Ctxt as -> Ctxt (a ': as)
infixr 5 :>

lookup :: Ix as a -> Ctxt as -> ValTy a
lookup IZ (x :> xs) = x
lookup (IS i) (_ :> xs) = lookup i xs

eval :: Term '[] a -> ValTy a
eval = eval' CNil

eval' :: Ctxt as -> Term as a -> ValTy a
eval' c (Var i) = lookup i c
eval' _ (Lit v) = v
eval' c (Lam _ _ body) = \x -> eval' (x :> c) body
eval' c (App f x) = eval' c f $ eval' c x
eval' _ None = Nothing
eval' c (Some x) = Just $ eval' c x
eval' _ Nil = []
eval' c (Cons x xs) = (eval' c x):(eval' c xs)
