{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Compile (
    JS(..)
  , NameCtxt(..)
  , compile
  , compile'
  , compileStmt
  , compileStmt'
  , compileProg
  , compileProg'
) where

import Prelude hiding (lookup)
import qualified Data.Text as T

import Language.JStlc.Types
import Language.JStlc.Syntax
import Language.JStlc.JS

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

-- TODO: consider alternatives
jsFix :: JS
jsFix = JSLambda "f"
  (JSCall
    (JSLambda "x" (JSCall
                    (JSVar "f")
                    [(JSLambda "y" (JSCall (JSCall (JSVar "x") [(JSVar "x")])
                                          [(JSVar "y")]))]))
    [(JSLambda "x" (JSCall
                    (JSVar "f")
                    [(JSLambda "y" (JSCall (JSCall (JSVar "x") [(JSVar "x")])
                                          [(JSVar "y")]))]))])

compile :: Term '[] a -> JS
compile = compile' CNil

compile' :: NameCtxt as -> Term as a -> JS
compile' c (Var i) = JSVar $ lookup i c
compile' _ (Lit v) = toJS v
compile' c (Lam x _ body) = JSLambda x (compile' (x :> c) body)
compile' c (App f x) = JSCall (compile' c f) [(compile' c x)]
compile' c (Let x t u) =
  JSCall (JSLambda x (compile' (x :> c) u)) [compile' c t]
compile' c (LetRec x ty t u) = -- TODO: optimize this
  compile' c (Let x (Fix (Lam x ty t)) u)
compile' c (Fix t) = JSCall jsFix [compile' c t]
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

compileStmt :: NameCtxt before -> Stmt before after -> JSStmt
compileStmt c s = fst $ compileStmt' c s

compileStmt' :: NameCtxt before -> Stmt before after -> (JSStmt, NameCtxt after)
compileStmt' c (Define f (Lam x _ body)) = 
  (JSFunDef f [x] (compile' (x :> c) body), f :> c)
compileStmt' c (Define x t) = (JSLet x (compile' c t), x :> c)
compileStmt' c (DefineRec f _ (Lam x _ body)) =
  (JSFunDef f [x] (compile' (x :> f :> c) body), f :> c)
compileStmt' c (DefineRec x ty t) =
  (JSLet x (compile' c (Fix (Lam x ty t))), x :> c)

compileProg :: Prog as -> JSProg
compileProg =  fst . compileProg'

compileProg' :: Prog as -> (JSProg, NameCtxt as)
compileProg' EmptyProg = ([], CNil)
compileProg' (p :&: s) = let (jsP, c) = compileProg' p
                             (jsS, c') = compileStmt' c s in
                             (jsP ++ [jsS], c')
