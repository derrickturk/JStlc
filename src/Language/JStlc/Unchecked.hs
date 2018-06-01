{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes #-}

module Language.JStlc.Unchecked (
    UBinOp(..)
  , UTerm(..)
  , UStmt(..)
  , UProg(..)
  , ExUProg(..)
) where

import qualified Data.Text as T

import Data.Nat
import Language.JStlc.Types
import Language.JStlc.JS

data UBinOp :: * where
  UAdd :: UBinOp
  USub :: UBinOp
  UMul :: UBinOp
  UDiv :: UBinOp
  UMod :: UBinOp
  UOr :: UBinOp
  UAnd :: UBinOp
  ULt :: UBinOp
  ULtEq :: UBinOp
  UGt :: UBinOp
  UGtEq :: UBinOp
  UStrCat :: UBinOp
  UAppend :: UBinOp
  UEq :: UBinOp

-- unchecked terms in a context of given depth
data UTerm :: Nat -> * where
  UVar :: T.Text -> UTerm n
  ULit :: (ISTy a, ToJS (ValTy a), Show (ValTy a)) => ValTy a -> UTerm n
  ULam :: T.Text -> STy a -> UTerm ('S n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n
  ULet :: T.Text -> UTerm n -> UTerm ('S n) -> UTerm n
  ULetRec :: T.Text -> STy a -> UTerm ('S n) -> UTerm ('S n) -> UTerm n
  UFix :: UTerm n -> UTerm n
  UNone :: STy a -> UTerm n
  USome :: UTerm n -> UTerm n
  UNil :: STy a -> UTerm n
  UCons :: UTerm n -> UTerm n -> UTerm n
  UBinOpApp :: UBinOp -> UTerm n -> UTerm n -> UTerm n
  UIfThenElse :: UTerm n -> UTerm n -> UTerm n -> UTerm n
  UFoldL :: UTerm n -> UTerm n -> UTerm n -> UTerm n
  UMap :: UTerm n -> UTerm n -> UTerm n

data UStmt :: Nat -> Nat -> * where
  UDefine :: T.Text -> UTerm n -> UStmt n ('S n)
  UDefineTyped :: T.Text -> STy a -> UTerm n -> UStmt n ('S n)
  UDefineRec :: T.Text -> STy a -> UTerm ('S n) -> UStmt n ('S n)

data UProg :: Nat -> * where
  UEmptyProg :: UProg 'Z
  (:&?:) :: UProg n -> UStmt n m -> UProg m
infixr 5 :&?:

newtype ExUProg =
  ExUProg { runExUProg :: forall r . (forall n . SNat n -> UProg n -> r) -> r }

instance Show UBinOp where
  show UAdd = "UAdd"
  show USub = "USub"
  show UMul = "UMul"
  show UDiv = "UDiv"
  show UMod = "UMod"
  show UOr = "UOr"
  show UAnd = "UAnd"
  show ULt = "ULt"
  show ULtEq = "ULtEq"
  show UGt = "UGt"
  show UGtEq = "UGtEq"
  show UStrCat = "UStrCat"
  show UAppend = "UAppend"
  show UEq = "UEq"

instance Show (UTerm n) where
  show (UVar x) = "UVar " ++ show x
  show (ULit v) = "ULit " ++ show v
  show (ULam x ty body) =
    "ULam " ++ show x ++ " " ++ show ty ++ " (" ++ show body ++ ")"
  show (UApp x y) = "UApp (" ++ show x ++ ") (" ++ show y ++ ")"
  show (ULet x t u) =
    "ULet " ++ show x ++ " (" ++ show t ++ ") (" ++ show u ++ ")"
  show (ULetRec x ty t u) =
    "ULetRec " ++ show x ++ " " ++ show ty ++
    " (" ++ show t ++ ") (" ++ show u ++ ")"
  show (UFix x) = "UFix (" ++ show x ++ ")"
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

instance Show (UStmt n m) where
  show (UDefine x t) = "UDefine " ++ show x ++ " (" ++ show t ++ ")"
  show (UDefineTyped x ty t) =
    "UDefineTyped " ++ show x ++ " " ++ show ty ++ " (" ++ show t ++ ")"
  show (UDefineRec x ty t) =
    "UDefineRec " ++ show x ++ " " ++ show ty ++ " (" ++ show t ++ ")"

instance Show (UProg n) where
  show UEmptyProg = "UEmptyProg"
  show (p :&?: s) = show p ++ " :&?: " ++ show s
