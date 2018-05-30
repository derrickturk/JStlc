{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes #-}

module Language.JStlc.Check (
    TypeError(..)
  , ExTerm(..)
  , check
) where

import qualified Data.Text as T

import Data.Nat
import Data.Type.Equality
import Language.JStlc.Unchecked
import Language.JStlc.Syntax

data TypeError :: * where
  Mismatch :: Ty -> Ty -> TypeError
  UndefinedVar :: T.Text -> TypeError
  ExpectedFnType :: Ty -> TypeError
  ExpectedOptionType :: Ty -> TypeError
  ExpectedListType :: Ty -> TypeError

-- this MUST be a newtype, because -XImpredicativeTypes blows up on
--   the type alias version
newtype ExTerm ctxt =
  ExTerm { runExTerm :: forall r . (forall a . STy a -> Term ctxt a -> r) -> r }

data STyCtxt :: [Ty] -> * where
  STyNil :: STyCtxt '[]
  (:::) :: STy a -> STyCtxt as -> STyCtxt (a ': as)
infixr 5 :::

data UNameCtxt :: Nat -> * where
  CNil :: UNameCtxt 'Z
  (:>) :: T.Text -> UNameCtxt n -> UNameCtxt ('S n)
infixr 5 :>

varIx :: UNameCtxt n -> T.Text -> Maybe (Ix as a)
varIx = undefined

check :: UTerm 'Z -> Either TypeError (ExTerm '[])
check = check' CNil STyNil

check' :: UNameCtxt n -> STyCtxt as -> UTerm n -> Either TypeError (ExTerm as)
{--
check' c (UVar x) = case varIx c x of
  Just i -> Right $ \k -> k (Var i)
  Nothing -> Left $ UndefinedVar x
--}
check' _ _ (ULit v) = Right $ ExTerm (\k -> k sTy (Lit v))

check' n c (ULam x ty body) = do
  exBody <- check' (x :> n) (ty ::: c) body
  runExTerm exBody $
    \sBody tBody -> Right $ ExTerm (\k -> k (SFnTy ty sBody) (Lam x ty tBody))

check' n c (UApp f x) = do
  exF <- check' n c f
  exX <- check' n c x
  runExTerm exF $
    \sF tF -> case sF of
      SFnTy sA sB -> runExTerm exX $
        \sX tX -> case testEquality sX sA of
          Just Refl -> Right $ ExTerm (\k -> k sB (App tF tX))
          _ -> Left $ Mismatch (unSTy sA) (unSTy sX)
      _ -> Left $ ExpectedFnType (unSTy sF)

check' _ _ (UNone ty@(SOptionTy _)) = Right $ ExTerm (\k -> k ty (None ty))
check' _ _ (UNone ty) = Left $ ExpectedOptionType (unSTy ty)

check' n c (USome x) = do
  exX <- check' n c x
  runExTerm exX $
    \s t -> Right (ExTerm (\k -> k (SOptionTy s) (Some t)))

check' _ _ (UNil ty@(SListTy _)) = Right $ ExTerm (\k -> k ty (Nil ty))
check' _ _ (UNil ty) = Left $ ExpectedListType (unSTy ty)

check' n c (UCons x xs) = do
  exX <- check' n c x
  exXs <- check' n c xs
  runExTerm exX $
    \s1 t1 -> runExTerm exXs $
      \s2 t2 -> case s2 of
        SListTy s2' -> case testEquality s1 s2' of
          Just Refl -> Right $ ExTerm (\k -> k (SListTy s2') (Cons t1 t2))
          _ -> Left $ Mismatch (unSTy s2') (unSTy s1)
        _ -> Left $ ExpectedListType (unSTy s2)

check' n c (UBinOpApp op x y) = do
  let aTy = argTy op
  exX <- check' n c x
  exY <- check' n c y
  runExTerm exX $
    \s1 t1 -> case testEquality s1 aTy of
      Just Refl -> runExTerm exY $
        \s2 t2 -> case testEquality s2 aTy of
          Just Refl -> Right $ ExTerm (\k -> k (resTy op) (BinOpApp op t1 t2))
          _ -> Left $ Mismatch (unSTy aTy) (unSTy s2)
      _ -> Left $ Mismatch (unSTy aTy) (unSTy s1)

check' n c (UIfThenElse b t f) = do
  exB <- check' n c b
  exT <- check' n c t
  exF <- check' n c f
  runExTerm exB $
    \sB tB -> case testEquality sB SBoolTy of
      Just Refl -> runExTerm exT $
        \sT tT -> runExTerm exF $
          \sF tF -> case testEquality sT sF of
            Just Refl -> Right $ ExTerm (\k -> k sT (IfThenElse tB tT tF))
            _ -> Left $ Mismatch (unSTy sT) (unSTy sF)
      _ -> Left $ Mismatch BoolTy (unSTy sB)

check' n c (UFoldL f x xs) = do
  exF <- check' n c f
  exX <- check' n c x
  exXs <- check' n c xs
  runExTerm exF $
    \sF tF -> case sF of
      SFnTy sA sBA -> case sBA of
        SFnTy sB sA' -> case testEquality sA' sA of
          Just Refl -> runExTerm exX $
            \sX tX -> case testEquality sX sA of
              Just Refl -> runExTerm exXs $
                \sXs tXs -> case sXs of
                  SListTy sElem -> case testEquality sElem sB of
                    Just Refl -> Right $ ExTerm (\k -> k sA (FoldL tF tX tXs)) 
                    _ -> Left $ Mismatch (ListTy (unSTy sB)) (unSTy sXs)
                  _ -> Left $ Mismatch (ListTy (unSTy sB)) (unSTy sXs)
              _ -> Left $ Mismatch (unSTy sA) (unSTy sX)
          _ -> Left $ Mismatch (FnTy (unSTy sB) (unSTy sA)) (unSTy sBA)
        _ -> Left $ ExpectedFnType (unSTy sBA)
      _ -> Left $ ExpectedFnType (unSTy sF)

argTy :: ISTy a => BinOp a b -> STy a
argTy _ = sTy

resTy :: ISTy b => BinOp a b -> STy b
resTy _ = sTy
