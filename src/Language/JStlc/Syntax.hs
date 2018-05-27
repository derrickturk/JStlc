{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.JStlc.Syntax (
    Ty(..)
  , STy(..)
  , STyI(..)
  , LitString(..)
  , ValTy(..)
  , Ix(..)
  , Term(..)
) where

import GHC.Exts (Constraint)

-- TODO: could be a lark to use a Vect for the type context
{--
data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
--}

data Ty :: * where
  IntTy :: Ty
  BoolTy :: Ty
  StringTy :: Ty
  FnTy :: Ty -> Ty -> Ty
  OptionTy :: Ty -> Ty
  ListTy :: Ty -> Ty

data STy :: Ty -> * where
  SIntTy :: STy IntTy 
  SBoolTy :: STy BoolTy 
  SStringTy :: STy StringTy 
  SFnTy :: STy a -> STy b -> STy (FnTy a b)
  SOptionTy :: STy a -> STy (OptionTy a)
  SListTy :: STy a -> STy (ListTy a)

-- for "implicit" type singletons
class STyI (a :: Ty) where
  sTy :: STy a
instance STyI IntTy where
  sTy = SIntTy
instance STyI BoolTy where
  sTy = SBoolTy
instance STyI StringTy where
  sTy = SStringTy
instance (STyI a, STyI b) => STyI (FnTy a b) where
  sTy = SFnTy sTy sTy
instance STyI a => STyI (OptionTy a) where
  sTy = SOptionTy sTy
instance STyI a => STyI (ListTy a) where
  sTy = SListTy sTy

newtype LitString = LitString { toString :: String }

type family ValTy (a :: Ty) = l | l -> a where
  ValTy 'IntTy = Int
  ValTy 'BoolTy = Bool
  ValTy 'StringTy = LitString
  ValTy ('FnTy a b) = ValTy a -> ValTy b
  ValTy ('OptionTy a) = Maybe (ValTy a)
  ValTy ('ListTy a) = [ValTy a]

data Ix :: [Ty] -> Ty -> * where
  IZ :: Ix (a ': as) a
  IS :: Ix as a -> Ix (b ': as) a

-- TODO: there's got to be an alternative to all these STyI constraints...
data Term :: [Ty] -> Ty -> * where
  Var :: STyI a => Ix ts a -> Term ts a
  Lit :: STyI a => ValTy a -> Term ts a
  Lam :: (STyI a, STyI b) =>
    String -> STy a -> Term (a ': ts) b -> Term ts (FnTy a b)
  App :: (STyI a, STyI b) =>
    Term ts (FnTy a b) -> Term ts a -> Term ts b
  None :: STyI a => Term ts (OptionTy a)
  Some :: STyI a => Term ts a -> Term ts (OptionTy a)
  Nil :: STyI a => Term ts (ListTy a)
  Cons :: STyI a => Term ts a -> Term ts (ListTy a) -> Term ts (ListTy a)
