{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, TypeFamilies, RankNTypes, TypeInType #-}

module Data.Nat (
    Nat(..)
  , SNat(..)
  , (:+:)
  , Fin(..)
  , SFin(..)
) where

import Data.Kind (Type)

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Show, Eq)

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data Fin :: Nat -> Type where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

data SFin :: forall (n :: Nat) . Fin n -> Type where
  SFZ :: SFin 'FZ
  SFS :: SFin f -> SFin ('FS f)

type family (:+:) (n :: Nat) (m :: Nat) = (r :: Nat) where
  n :+: 'Z = n
  n :+: ('S k) = 'S (n :+: k)

deriving instance Show (SNat n)
deriving instance Show (Fin n)
deriving instance Show (SFin f)

deriving instance Eq (SNat n)
deriving instance Eq (Fin n)
deriving instance Eq (SFin f)
