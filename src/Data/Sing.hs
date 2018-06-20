{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, TypeInType #-}

module Data.Sing (
    Sing
  , ISing(..)
  , SingKind(..)
  , ExSing(..)
  , SVect(..)
  , svlookup
) where

import Data.Kind (Type)

import Data.Nat
import Data.Vect

data family Sing (a :: k) :: Type

class ISing (a :: k) where
  sing :: Sing a

class SingKind k where
  type UnSing k = (r :: Type) | r -> k
  unsing :: Sing (a :: k) -> UnSing k
  toExSing :: UnSing k -> ExSing k

newtype ExSing k =
  ExSing { runExSing :: forall r . (forall (a :: k) . Sing a -> r) -> r }

data SVect :: forall (n :: Nat) . Vect n k -> Type where
  SVNil :: SVect 'VNil
  (:->) :: Sing a -> SVect as -> SVect (a ':> as)
infixr 5 :->

svlookup :: forall (n :: Nat) (as :: Vect n Type) (f :: Fin n)
          . SFin f -> SVect as -> Sing (VLookup f as)
svlookup SFZ (x :-> _) = x
svlookup (SFS f) (_ :-> xs) = svlookup f xs
