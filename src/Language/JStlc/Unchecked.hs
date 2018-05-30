{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Language.JStlc.Unchecked (
    UTerm(..)
  , BinOp
) where

import qualified Data.Text as T

import Data.Nat
import Language.JStlc.Types
import Language.JStlc.Syntax (BinOp)
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

instance Show (UTerm n) where
  show (UVar x) = "UVar " ++ show x
  show (ULit v) = "ULit " ++ show v
  show (ULam x ty body) =
    "ULam " ++ show x ++ " " ++ show ty ++ " (" ++ show body ++ ")"
  show (UApp x y) = "UApp (" ++ show x ++ ") (" ++ show y ++ ")"
  show (UNone ty) = "UNone " ++ show ty
  show (USome x) = "USome (" ++ show x ++ ")"
  show (UNil ty) = "UNil " ++ show ty
  show (UCons x xs) = "UCons (" ++ show x ++ ") (" ++ show xs ++ ")"
  show (UBinOpApp op x y) =
    "UBinOpApp " ++ show op ++ " (" ++ show x ++ ") (" ++ show y ++ ")"
  show (UIfThenElse cond t f) =
    "UIfThenElse (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show f ++ ")"
  show (UFoldL f x xs) =
    "UFoldL (" ++ show f ++ ") (" ++ show x ++ ") (" ++ show xs ++ ")"
  show (UMap f x) = "UMap (" ++ show f ++ ") (" ++ show x ++ ")"
