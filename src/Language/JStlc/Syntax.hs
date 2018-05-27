{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.JStlc.Syntax (
    Ty(..)
  , STy(..)
  , LitString(..)
  , ValTy(..)
  , Ix(..)
  , Term(..)
) where

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

data Term :: [Ty] -> Ty -> * where
  Var :: Ix ts a -> Term ts a
  Lit :: ValTy a -> Term ts a
  Lam :: String -> STy a -> Term (a ': ts) b -> Term ts (FnTy a b)
  App :: Term ts (FnTy a b) -> Term ts a -> Term ts b
  None :: Term ts (OptionTy a)
  Some :: Term ts a -> Term ts (OptionTy a)
  Nil :: Term ts (ListTy a)
  Cons :: Term ts a -> Term ts (ListTy a) -> Term ts (ListTy a)
