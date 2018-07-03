{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, TypeInType #-}
{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Types (
    Ty(..)
  , Sing(..)
  , STy
  , ValTy
  , TyCtxt
  , STyCtxt
  , NameCtxt
  , Ctxt
  , ValTyVect
  , showVal
) where

import Data.Kind (Type)
import Data.Type.Equality
import Data.Monoid ((<>))
import qualified Data.Text as T

import Data.Nat
import Data.Vect
import Data.Sing

import Language.JStlc.Pretty.Class

data Ty :: Type where
  IntTy :: Ty
  BoolTy :: Ty
  StringTy :: Ty
  FnTy :: Ty -> Ty -> Ty
  OptionTy :: Ty -> Ty
  ListTy :: Ty -> Ty

data instance Sing (a :: Ty) where
  SIntTy :: Sing 'IntTy 
  SBoolTy :: Sing 'BoolTy 
  SStringTy :: Sing 'StringTy 
  SFnTy :: Sing a -> Sing b -> Sing ('FnTy a b)
  SOptionTy :: Sing a -> Sing ('OptionTy a)
  SListTy :: Sing a -> Sing ('ListTy a)

type STy (a :: Ty) = Sing a

instance ISing 'IntTy where
  sing = SIntTy
instance ISing 'BoolTy where
  sing = SBoolTy
instance ISing 'StringTy where
  sing = SStringTy
instance (ISing a, ISing b) => ISing ('FnTy a b) where
  sing = SFnTy sing sing
instance ISing a => ISing ('OptionTy a) where
  sing = SOptionTy sing
instance ISing a => ISing ('ListTy a) where
  sing = SListTy sing

instance SingKind Ty where
  type UnSing Ty = Ty

  unsing SIntTy = IntTy
  unsing SBoolTy = BoolTy
  unsing SStringTy = StringTy
  unsing (SFnTy a b) = FnTy (unsing a) (unsing b)
  unsing (SOptionTy a) = OptionTy (unsing a)
  unsing (SListTy a) = ListTy (unsing a)

  toExSing IntTy = ExSing ($ SIntTy)
  toExSing BoolTy = ExSing ($ SBoolTy)
  toExSing StringTy = ExSing ($ SStringTy)
  toExSing (FnTy a b) = runExSing (toExSing a) $ \sA ->
    runExSing (toExSing b) $ \sB -> ExSing ($ (SFnTy sA sB))
  toExSing (OptionTy a) = runExSing (toExSing a) $ \sA ->
    ExSing ($ (SOptionTy sA))
  toExSing (ListTy a) = runExSing (toExSing a) $ \sA ->
    ExSing ($ (SListTy sA))

-- P R A I S E  T H E  C  U  S  K
instance TestEquality (Sing :: Ty -> Type) where
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
type STyCtxt (as :: TyCtxt n) = Sing as
type NameCtxt (as :: TyCtxt n) = Vect (VLength as) T.Text
type Ctxt as = HVect (ValTyVect as)

type family ValTyVect (as :: TyCtxt n) = (r :: Vect n Type) | r -> as where
  ValTyVect 'VNil = 'VNil
  ValTyVect (a ':> as) = ValTy a ':> ValTyVect as

instance Show Ty where
  show IntTy = "IntTy"
  show BoolTy = "BoolTy"
  show StringTy = "StringTy"
  show (FnTy a b) = "FnTy (" ++ show a ++ ") (" ++ show b ++ ")"
  show (OptionTy a) = "OptionTy (" ++ show a ++ ")"
  show (ListTy a) = "ListTy (" ++ show a ++ ")"

instance ShowSing Ty where
  showSing SIntTy = "SIntTy"
  showSing SBoolTy = "SBoolTy"
  showSing SStringTy = "SStringTy"
  showSing (SFnTy a b) = "SFnTy (" ++ showSing a ++ ") (" ++ showSing b ++ ")"
  showSing (SOptionTy a) = "SOptionTy (" ++ showSing a ++ ")"
  showSing (SListTy a) = "SListTy (" ++ showSing a ++ ")"

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
  pretty = pretty . unsing
