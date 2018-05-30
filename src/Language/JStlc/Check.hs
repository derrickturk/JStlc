{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes #-}

module Language.JStlc.Check (
    TypeError(..)
  , ExTerm(..)
  , check
) where

import qualified Data.Text as T

import Data.Nat
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
check' n c (USome x) = case check' n c x of
  Left e -> Left e
  Right exT -> runExTerm exT $
    \s t -> Right (ExTerm (\k -> k (SOptionTy s) (Some t)))
