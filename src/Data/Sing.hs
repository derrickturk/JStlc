{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, TypeInType #-}

module Data.Sing (
    Sing
  , SVect(..)
  , svlookup
) where

import Data.Kind (Type)

import Data.Nat
import Data.Vect

data family Sing (a :: k) :: Type

data SVect :: forall (n :: Nat) . Vect n k -> Type where
  SVNil :: SVect 'VNil
  (:->) :: Sing a -> SVect as -> SVect (a ':> as)
infixr 5 :->

svlookup :: forall (n :: Nat) (as :: Vect n Type) (f :: Fin n)
          . SFin f -> SVect as -> Sing (VLookup f as)
svlookup SFZ (x :-> _) = x
svlookup (SFS f) (_ :-> xs) = svlookup f xs
