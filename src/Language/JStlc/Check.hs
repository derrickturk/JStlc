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
        SListTy s2' -> case eqSTy s1 s2' of
          Just Refl -> Right $ ExTerm (\k -> k (SListTy s2') (Cons t1 t2))
          _ -> Left $ Mismatch (unSTy s2') (unSTy s1)
        _ -> Left $ ExpectedListType (unSTy s2)
