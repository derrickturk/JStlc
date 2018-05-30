{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Data.Nat (
    Nat(..)
  , SNat(..)
  , Fin(..)
) where

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat

data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data Fin :: Nat -> * where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)
