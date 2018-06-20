{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes, TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Compile (
    JS(..)
  , compile
  , compile'
  , compileStmt
  , compileStmt'
  , compileProg
  , compileProg'
) where

import Prelude hiding (lookup)
import qualified Data.Text as T

import Data.Nat
import Data.Vect

import Language.JStlc.Types
import Language.JStlc.Syntax
import Language.JStlc.JS

lookup :: Ix as a -> NameCtxt as -> T.Text
lookup IZ (x :> _) = x
lookup (IS i) (_ :> xs) = lookup i xs

jsOpName :: BinOp a b -> T.Text
jsOpName Add = "+"
jsOpName Sub = "-"
jsOpName Mul = "*"
jsOpName Div = error "ICE"
jsOpName Mod = "%"
jsOpName Or = "||"
jsOpName And = "&&"
jsOpName Lt = "<"
jsOpName LtEq = "<="
jsOpName Gt = ">"
jsOpName GtEq = ">="
jsOpName StrCat = "+"
jsOpName Append = error "ICE"
jsOpName Eq = "==="

-- TODO: design compilation so that names don't exist in Vars
--   but get pulled from binders in a checkable way
jsFix :: JS n
jsFix = JSLambda "f"
  (JSCall
    (JSLambda "x"
      (JSCall (JSVar (FS FZ) "f")
              [(JSLambda "y"
                 (JSCall (JSCall (JSVar (FS FZ) "x") [(JSVar (FS FZ) "x")])
                         [(JSVar FZ "y")]))]))
    [(JSLambda "x"
      (JSCall (JSVar (FS FZ) "f")
              [(JSLambda "y"
                 (JSCall (JSCall (JSVar (FS FZ) "x") [(JSVar (FS FZ) "x")])
                         [(JSVar FZ "y")]))]))])

compile :: Term 'VNil a -> JS 'Z
compile = compile' VNil

compile' :: NameCtxt as -> Term as a -> JS (VLength as)
compile' c (Var i) = JSVar (toFin i) (lookup i c)
compile' _ (Lit v) = toJS v
compile' c (Lam x _ body) = JSLambda x (compile' (x :> c) body)
compile' c (App f x) = JSCall (compile' c f) [(compile' c x)]
compile' c (Let x t u) =
  JSCall (JSLambda x (compile' (x :> c) u)) [compile' c t]
compile' c (LetRec x ty t u) = -- TODO: optimize this
  compile' c (Let x (Fix (Lam x ty t)) u)
compile' c (Fix t) = JSCall jsFix [compile' c t]
compile' _ (None _) = JSBuiltIn "null"
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
  JSCondExpr (JSBinOpApp "!==" x' (JSBuiltIn "null"))
             (JSCall (compile' c f) [x'])
             x'
compile' c (MapList f x) = JSMethod (compile' c x) "map" [(compile' c f)]

compileStmt :: NameCtxt before
            -> Stmt before after
            -> JSStmt (VLength before) (VLength after)
compileStmt c s = fst $ compileStmt' c s

compileStmt' :: NameCtxt before
             -> Stmt before after
             -> (JSStmt (VLength before) (VLength after), NameCtxt after)
compileStmt' c (Define f (Lam x _ body)) = 
  (JSFunDef f (x :> VNil) (compile' (x :> c) body), f :> c)
compileStmt' c (Define x t) = (JSLet x (compile' c t), x :> c)
compileStmt' c (DefineRec f _ (Lam x _ body)) =
  (JSFunDefRec f (x :> VNil) (compile' (x :> f :> c) body), f :> c)
compileStmt' c (DefineRec x ty t) =
  (JSLet x (compile' c (Fix (Lam x ty t))), x :> c)

compileProg :: Prog as -> JSProg (VLength as)
compileProg =  fst . compileProg'

compileProg' :: Prog as -> (JSProg (VLength as), NameCtxt as)
compileProg' EmptyProg = (JSEmptyProg, VNil)
compileProg' (p :&: s) = let (jsP, c) = compileProg' p
                             (jsS, c') = compileStmt' c s in
                             (jsP :&&: jsS, c')
