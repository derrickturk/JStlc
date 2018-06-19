{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE RankNTypes, TypeInType #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Data.Vect (
    Vect(..)
  , vlookup
  , HVect(..)
  , hvlookup
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

-- not sure why I couldn't get away with an ordinary type family here

class VLookupable (f :: Fin n) (as :: Vect n Type) where
  type VLookup f as

instance VLookupable 'FZ (a ':> as) where
  type VLookup 'FZ (a ':> as) = a

instance VLookupable f as => VLookupable ('FS f) (a ':> as) where
  type VLookup ('FS f) (a ':> as) = VLookup f as

hvlookup :: forall (n :: Nat) (as :: Vect n Type) (f :: Fin n)
          . SFin f -> HVect as -> VLookup f as
hvlookup SFZ (x ::> _) = x
hvlookup (SFS f) (_ ::> xs) = hvlookup f xs
