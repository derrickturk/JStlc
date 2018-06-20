{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, TypeInType #-}

module Data.Sing (
    Sing(..)
  , ISing(..)
  , SingKind(..)
  , ExSing(..)
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

data instance Sing (a :: Vect n k) where
  SVNil :: Sing 'VNil
  (:->) :: Sing a -> Sing as -> Sing (a ':> as)
infixr 5 :->

instance ISing 'VNil where
  sing = SVNil
instance (ISing a, ISing as) => ISing (a ':> as) where
  sing = sing :-> sing

instance SingKind k => SingKind (Vect n k) where
  type UnSing (Vect n k) = Vect n (UnSing k)

  unsing SVNil = VNil
  unsing (x :-> xs) = unsing x :> unsing xs

  toExSing VNil = ExSing $ \k -> k SVNil
  toExSing (x :> xs) = let exX = toExSing x
                           exXs = toExSing xs in
    runExSing exX (\sX ->
      runExSing exXs (\sXs ->
        ExSing (\k -> k (sX :-> sXs))))

svlookup :: forall (n :: Nat) (as :: Vect n k) (f :: Fin n)
          . SFin f -> Sing as -> Sing (VLookup f as)
svlookup SFZ (x :-> _) = x
svlookup (SFS f) (_ :-> xs) = svlookup f xs
