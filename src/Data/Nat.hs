{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
{-# LANGUAGE RankNTypes, TypeInType #-}

module Data.Nat (
    Nat(..)
  , SNat(..)
  , Fin(..)
  , SFin(..)
) where

import Data.Kind (Type)

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data Fin :: Nat -> Type where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

data SFin :: forall (n :: Nat) . Fin n -> Type where
  SFZ :: SFin 'FZ
  SFS :: SFin f -> SFin ('FS f)
