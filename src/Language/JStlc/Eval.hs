{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.JStlc.Eval (
    Ctxt(..)
  , eval
  , eval'
  , evalStmt
  , evalProg
) where

import Prelude hiding (lookup)
import Data.Monoid ((<>))

import Language.JStlc.Types
import Language.JStlc.Syntax

data Ctxt :: [Ty] -> * where
  CNil :: Ctxt '[]
  (:>) :: ValTy a -> Ctxt as -> Ctxt (a ': as)
infixr 5 :>

lookup :: Ix as a -> Ctxt as -> ValTy a
lookup IZ (x :> _) = x
lookup (IS i) (_ :> xs) = lookup i xs

eval :: Term '[] a -> ValTy a
eval = eval' CNil

eval' :: Ctxt as -> Term as a -> ValTy a
eval' c (Var i) = lookup i c
eval' _ (Lit v) = v
eval' c (Lam _ _ body) = \x -> eval' (x :> c) body
eval' c (App f x) = eval' c f $ eval' c x
eval' c (Let _ t u) = let x = eval' c t in eval' (x :> c) u
eval' c (LetRec x ty t u) = eval' c (Let x (Fix (Lam x ty t)) u)
eval' c (Fix t) = eval' c t $ eval' c (Fix t)
eval' _ (None _) = Nothing
eval' c (Some x) = Just $ eval' c x
eval' _ (Nil _) = []
eval' c (Cons x xs) = (eval' c x):(eval' c xs)
eval' c (BinOpApp op x y) = evalBinOp op (eval' c x) (eval' c y)
eval' c (IfThenElse cond t f) = if eval' c cond then eval' c t else eval' c f
eval' c (FoldL f x xs) = foldl (eval' c f) (eval' c x) (eval' c xs)
eval' c (MapOption f x) = fmap (eval' c f) (eval' c x)
eval' c (MapList f x) = fmap (eval' c f) (eval' c x)

evalBinOp :: BinOp a b -> ValTy a -> ValTy a -> ValTy b
evalBinOp Add x y = x + y
evalBinOp Sub x y = x - y
evalBinOp Mul x y = x * y
evalBinOp Div x y = x `div` y
evalBinOp Or x y = x || y -- saved by laziness
evalBinOp And x y = x && y
evalBinOp StrCat x y = x <> y
evalBinOp Append x y = x <> y
evalBinOp Eq x y = x == y

evalStmt :: Ctxt as -> Stmt as bs -> Ctxt bs
evalStmt c (Define _ t) = eval' c t :> c
evalStmt c (DefineRec x ty t) = eval' c (Fix (Lam x ty t)) :> c

evalProg :: Prog as -> Ctxt as
evalProg EmptyProg = CNil
evalProg (p :&: s) = evalStmt (evalProg p) s
