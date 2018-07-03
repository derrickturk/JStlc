{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, TypeInType #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module Data.Sing (
    Sing
  , ISing(..)
  , SingKind(..)
  , ShowSing(..)
  , ExSing(..)
) where

import Data.Kind (Type)

data family Sing (a :: k) :: Type

class ISing (a :: k) where
  sing :: Sing a

class SingKind k where
  type UnSing k = (r :: Type) | r -> k
  unsing :: Sing (a :: k) -> UnSing k
  toExSing :: UnSing k -> ExSing k

-- TODO: precedence uggggggh, ShowS difflists ugggh
class ShowSing k where
  showSing :: Sing (a :: k) -> String

instance ShowSing k => Show (Sing (a :: k)) where
  show = showSing

newtype ExSing k =
  ExSing { runExSing :: forall r . (forall (a :: k) . Sing a -> r) -> r }
