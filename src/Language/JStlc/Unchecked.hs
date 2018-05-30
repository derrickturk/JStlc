{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Language.JStlc.Unchecked (
    UTerm(..)
) where

import qualified Data.Text as T

import Data.Nat
import Language.JStlc.Syntax
import Language.JStlc.JS

-- unchecked terms in a context of given depth
data UTerm :: Nat -> * where
  UVar :: T.Text -> UTerm n
  ULit :: (ISTy a, ToJS (ValTy a), Show (ValTy a)) => ValTy a -> UTerm n
  ULam :: T.Text -> STy a -> UTerm ('S n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n
  UNone :: STy a -> UTerm n
  USome :: UTerm n -> UTerm n
  UNil :: STy a -> UTerm n
  UCons :: UTerm n -> UTerm n -> UTerm n
  UBinOpApp :: (ISTy a, ISTy b) => BinOp a b -> UTerm n -> UTerm n -> UTerm n
  UIfThenElse :: UTerm n -> UTerm n -> UTerm n -> UTerm n
  UFoldL :: UTerm n -> UTerm n -> UTerm n -> UTerm n
  UMap :: UTerm n -> UTerm n -> UTerm n
