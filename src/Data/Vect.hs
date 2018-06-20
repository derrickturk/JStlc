{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, TypeInType #-}

module Data.Vect (
    Vect(..)
  , vlookup
  , HVect(..)
  , hvlookup
  , VLookup
  , VLength
) where

import Data.Kind (Type)

import Data.Nat

data Vect :: Nat -> Type -> Type where
  VNil :: Vect 'Z a
  (:>) :: a -> Vect n a -> Vect ('S n) a
infixr 5 :>

vlookup :: Fin n -> Vect n a -> a
vlookup FZ (x :> _) = x
vlookup (FS f) (_ :> xs) = vlookup f xs

-- behold the C U S K
data HVect :: forall (n :: Nat) . Vect n Type -> Type where
  HVNil :: HVect 'VNil
  (::>) :: a -> HVect as -> HVect (a ':> as)
infixr 5 ::>

hvlookup :: forall (n :: Nat) (as :: Vect n Type) (f :: Fin n)
          . SFin f -> HVect as -> VLookup f as
hvlookup SFZ (x ::> _) = x
hvlookup (SFS f) (_ ::> xs) = hvlookup f xs

type family VLookup (f :: Fin n) (as :: Vect n k) = (r :: k) where
  VLookup 'FZ (a ':> _) = a
  VLookup ('FS f) (_ ':> as) = VLookup f as

-- TODO: there's got to be an easier way to "reflect" n
type family VLength (as :: Vect n a) = (m :: Nat) where
  VLength 'VNil = 'Z
  VLength (_ ':> as) = 'S (VLength as)

deriving instance Show a => Show (Vect n a)
deriving instance Eq a => Eq (Vect n a)
deriving instance Functor (Vect n)

instance Foldable (Vect n) where
  foldMap _ VNil = mempty
  foldMap f (x :> xs) = f x `mappend` foldMap f xs

instance Show (HVect 'VNil) where
  show HVNil = "HVNil"
instance (Show a, Show (HVect as)) => Show (HVect (a ':> as)) where
  show (x ::> xs) = show x ++ " ::> " ++ show xs

instance Eq (HVect 'VNil) where
  HVNil == HVNil = True
instance (Eq a, Eq (HVect as)) => Eq (HVect (a ':> as)) where
  (x ::> xs) == (y ::> ys) = x == y && xs == ys
