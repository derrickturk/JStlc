{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Types (
    Ty(..)
  , STy(..)
  , ISTy(..)
  , unSTy
  , ExSTy
  , runExSTy
  , toExSTy
  , ValTy
  , TyCtxt
  , NameCtxt
  , Ctxt
  , ValTyVect
  , STyVect
  , showVal
) where

import Data.Kind (Type)
import Data.Type.Equality
import Data.Monoid ((<>))
import qualified Data.Text as T

import Data.Nat
import Data.Vect

import Language.JStlc.Pretty.Class

data Ty :: Type where
  IntTy :: Ty
  BoolTy :: Ty
  StringTy :: Ty
  FnTy :: Ty -> Ty -> Ty
  OptionTy :: Ty -> Ty
  ListTy :: Ty -> Ty

data STy :: Ty -> Type where
  SIntTy :: STy 'IntTy 
  SBoolTy :: STy 'BoolTy 
  SStringTy :: STy 'StringTy 
  SFnTy :: STy a -> STy b -> STy ('FnTy a b)
  SOptionTy :: STy a -> STy ('OptionTy a)
  SListTy :: STy a -> STy ('ListTy a)

class ISTy a where
  sTy :: STy a
instance ISTy 'IntTy where
  sTy = SIntTy
instance ISTy 'BoolTy where
  sTy = SBoolTy
instance ISTy 'StringTy where
  sTy = SStringTy
instance (ISTy a, ISTy b) => ISTy ('FnTy a b) where
  sTy = SFnTy sTy sTy
instance ISTy a => ISTy ('OptionTy a) where
  sTy = SOptionTy sTy
instance ISTy a => ISTy ('ListTy a) where
  sTy = SListTy sTy

unSTy :: STy a -> Ty
unSTy SIntTy = IntTy
unSTy SBoolTy = BoolTy
unSTy SStringTy = StringTy
unSTy (SFnTy a b) = FnTy (unSTy a) (unSTy b)
unSTy (SOptionTy a) = OptionTy (unSTy a)
unSTy (SListTy a) = ListTy (unSTy a)

newtype ExSTy = ExSTy { runExSTy :: forall r . (forall a . STy a -> r) -> r }

toExSTy :: Ty -> ExSTy
toExSTy IntTy = ExSTy ($ SIntTy)
toExSTy BoolTy = ExSTy ($ SBoolTy)
toExSTy StringTy = ExSTy ($ SStringTy)
toExSTy (FnTy a b) = runExSTy (toExSTy a) $ \sA ->
  runExSTy (toExSTy b) $ \sB -> ExSTy ($ (SFnTy sA sB))
toExSTy (OptionTy a) = runExSTy (toExSTy a) $ \sA -> ExSTy ($ (SOptionTy sA))
toExSTy (ListTy a) = runExSTy (toExSTy a) $ \sA -> ExSTy ($ (SListTy sA))

instance TestEquality STy where
  testEquality SIntTy SIntTy = Just Refl
  testEquality SBoolTy SBoolTy = Just Refl
  testEquality SStringTy SStringTy = Just Refl
  testEquality (SFnTy a b) (SFnTy c d)
    | Just Refl <- testEquality a c, Just Refl <- testEquality b d = Just Refl
  testEquality (SOptionTy a) (SOptionTy b)
    | Just Refl <- testEquality a b = Just Refl
  testEquality (SListTy a) (SListTy b)
    | Just Refl <- testEquality a b = Just Refl
  testEquality _ _ = Nothing

type family ValTy (a :: Ty) = v | v -> a where
  ValTy 'IntTy = Integer
  ValTy 'BoolTy = Bool
  ValTy 'StringTy = T.Text
  ValTy ('FnTy a b) = ValTy a -> ValTy b
  ValTy ('OptionTy a) = Maybe (ValTy a)
  ValTy ('ListTy a) = [ValTy a]

type TyCtxt (n :: Nat) = Vect n Ty
type NameCtxt (as :: TyCtxt n) = Vect (VLength as) T.Text
type Ctxt as = HVect (ValTyVect as)

type family ValTyVect (as :: TyCtxt n) = (r :: Vect n Type) | r -> as where
  ValTyVect 'VNil = 'VNil
  ValTyVect (a ':> as) = ValTy a ':> ValTyVect as

type family STyVect (as :: TyCtxt n) = (r :: Vect n Type) | r -> as where
  STyVect 'VNil = 'VNil
  STyVect (a ':> as) = STy a ':> STyVect as

instance Show Ty where
  show IntTy = "IntTy"
  show BoolTy = "BoolTy"
  show StringTy = "StringTy"
  show (FnTy a b) = "FnTy (" ++ show a ++ ") (" ++ show b ++ ")"
  show (OptionTy a) = "OptionTy (" ++ show a ++ ")"
  show (ListTy a) = "ListTy (" ++ show a ++ ")"

instance Show (STy a) where
  show SIntTy = "SIntTy"
  show SBoolTy = "SBoolTy"
  show SStringTy = "SStringTy"
  show (SFnTy a b) = "SFnTy (" ++ show a ++ ") (" ++ show b ++ ")"
  show (SOptionTy a) = "SOptionTy (" ++ show a ++ ")"
  show (SListTy a) = "SListTy (" ++ show a ++ ")"

-- used to "render" value types e.g. in REPL, where possible
showVal :: STy a -> ValTy a -> String
showVal SIntTy n = show n
showVal SBoolTy b = show b
showVal SStringTy s = show s
showVal (SFnTy _ _) _ = "<function>"
showVal (SOptionTy _) Nothing = "Nothing"
showVal (SOptionTy a) (Just x) = "Just (" ++ showVal a x ++ ")"
showVal (SListTy _) [] = "[]"
showVal (SListTy a) (x:xs) = "[" ++ showVal a x ++ go a xs ++ "]" where
  go _ [] = ""
  go t (y:ys) = ", " ++ showVal t y ++ go t ys

instance Pretty Ty where
  pretty IntTy = "Int"
  pretty BoolTy = "Bool"
  pretty StringTy = "String"
  pretty (FnTy a b) = pretty a <> " -> " <> pretty b
  pretty (OptionTy a) = T.cons '?' $ pretty a
  pretty (ListTy a) = T.cons '[' $ T.snoc (pretty a) ']'

instance Pretty (STy a) where
  pretty = pretty . unSTy
