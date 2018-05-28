{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Language.JStlc.Syntax (
    Ty(..)
  , STy(..)
  , LitString(..)
  , ValTy(..)
  , Ix(..)
  , Term(..)
) where

import Language.JStlc.Internal
import Language.JStlc.JS

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

type family ValTy (a :: Ty) = v | v -> a where
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
  Lit :: (Show (ValTy a), ToJS (ValTy a)) => ValTy a -> Term ts a
  Lam :: String -> STy a -> Term (a ': ts) b -> Term ts (FnTy a b)
  App :: Term ts (FnTy a b) -> Term ts a -> Term ts b
  None :: Term ts (OptionTy a)
  Some :: Term ts a -> Term ts (OptionTy a)
  Nil :: Term ts (ListTy a)
  Cons :: Term ts a -> Term ts (ListTy a) -> Term ts (ListTy a)

instance Show (STy a) where
  show SIntTy = "SIntTy"
  show SBoolTy = "SBoolTy"
  show SStringTy = "SStringTy"
  show (SFnTy a b) = "SFnTy (" ++ show a ++ ") (" ++ show b ++ ")"
  show (SOptionTy a) = "SOptionTy (" ++ show a ++ ")"
  show (SListTy a) = "SOptionTy (" ++ show a ++ ")"

instance Show (Ix as a) where
  show = show . toInt where
    toInt :: Ix as a -> Int
    toInt IZ = 0
    toInt (IS i) = 1 + toInt i

instance Show (Term as a) where
  show (Var i) = "Var " ++ show i
  show (Lit v) = show v
  show (Lam x ty body) =
    "Lam " ++ x ++ " " ++ show ty ++ " (" ++ show body ++ ")"
  show (App x y) = "App (" ++ show x ++ ") (" ++ show y ++ ")"
  show None = "None"
  show (Some x) = "Some (" ++ show x ++ ")"
  show Nil = "Nil"
  show (Cons x xs) = "Cons (" ++ show x ++ ") (" ++ show xs ++ ")"
