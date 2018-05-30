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
import qualified Data.Text as T

import Language.JStlc.JS
import Language.JStlc.Syntax

data NameCtxt :: [Ty] -> * where
  CNil :: NameCtxt '[]
  (:>) :: T.Text -> NameCtxt as -> NameCtxt (a ': as)
infixr 5 :>

lookup :: Ix as a -> NameCtxt as -> T.Text
lookup IZ (x :> _) = x
lookup (IS i) (_ :> xs) = lookup i xs

jsOpName :: BinOp a b -> T.Text
jsOpName Add = "+"
jsOpName Sub = "-"
jsOpName Mul = "*"
jsOpName Div = error "ICE"
jsOpName Or = "||"
jsOpName And = "&&"
jsOpName StrCat = "+"
jsOpName Append = error "ICE"
jsOpName Eq = "==="

compile :: Term '[] a -> JS
compile = compile' CNil

compile' :: NameCtxt as -> Term as a -> JS
compile' c (Var i) = JSVar $ lookup i c
compile' _ (Lit v) = toJS v
compile' c (Lam x _ body) = JSLambda x (compile' (x :> c) body)
compile' c (App f x) = JSCall (compile' c f) [(compile' c x)]
compile' _ (None _) = JSVar "null"
compile' c (Some t) = compile' c t
compile' _ (Nil _) = JSArray []
compile' c (Cons x (Nil _)) = JSArray [compile' c x]
compile' c (Cons x xs) =
  JSMethod (JSArray [compile' c x]) "concat" [(compile' c xs)]
compile' c (BinOpApp op x y) = case op of
  Div -> JSUnOpApp "~" $ JSUnOpApp "~" $ JSBinOpApp "/"
           (compile' c x) (compile' c y)
  Append -> JSMethod (compile' c x) "concat" [(compile' c y)]
  _ -> JSBinOpApp (jsOpName op) (compile' c x) (compile' c y)
compile' c (IfThenElse cond t f) =
  JSCondExpr (compile' c cond) (compile' c t) (compile' c f)
compile' c (FoldL f x xs) =
  JSMethod (compile' c xs) "reduce" [compile' c f, compile' c x]
compile' c (MapOption f x) = let x' = compile' c x in
  JSCondExpr (JSBinOpApp "!==" x' (JSVar "null")) (JSCall (compile' c f) [x']) x'
compile' c (MapList f x) = JSMethod (compile' c x) "map" [(compile' c f)]
