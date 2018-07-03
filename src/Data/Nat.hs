{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, TypeFamilies, RankNTypes, TypeInType #-}

module Data.Nat (
    Nat(..)
  , (:+:)
  , Fin(..)
  , SNat
  , SFin
  , Sing(..)
) where

import Data.Kind (Type)

import Data.Sing

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Show, Eq)

type family (:+:) (n :: Nat) (m :: Nat) = (r :: Nat) where
  n :+: 'Z = n
  n :+: ('S k) = 'S (n :+: k)

type SNat (n :: Nat) = Sing n

data Fin :: Nat -> Type where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Show (Fin n)
deriving instance Eq (Fin n)

type SFin (k :: Fin n) = Sing k

data instance Sing (n :: Nat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

instance ISing 'Z where
  sing = SZ
instance ISing n => ISing ('S n) where
  sing = SS sing

instance SingKind Nat where
  type UnSing Nat = Nat

  unsing SZ = Z
  unsing (SS n) = S (unsing n)

  toExSing Z = ExSing $ \k -> k SZ
  toExSing (S n) = let exN = toExSing n in
    runExSing exN (\sN -> ExSing (\k -> k (SS sN)))

data instance Sing (k :: Fin n) where
  SFZ :: Sing 'FZ
  SFS :: Sing k -> Sing ('FS k)

instance ISing 'FZ where
  sing = SFZ
instance ISing k => ISing ('FS k) where
  sing = SFS sing

instance SingKind (Fin n) where
  type UnSing (Fin n) = Fin n

  unsing SFZ = FZ
  unsing (SFS k) = FS (unsing k)

  toExSing FZ = ExSing $ \k -> k SFZ
  toExSing (FS n) = let exN = toExSing n in
    runExSing exN (\sN -> ExSing (\k -> k (SFS sN)))

instance ShowSing Nat where
  showSing SZ = "SZ"
  showSing (SS n) = "SS (" ++ showSing n ++ ")"

deriving instance Eq (Sing (n :: Nat))

instance ShowSing (Fin n) where
  showSing SFZ = "SFZ"
  showSing (SFS k) = "SFS (" ++ showSing k ++ ")"

deriving instance Eq (Sing (k :: Fin n))
