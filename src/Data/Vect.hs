{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, TypeInType, TypeFamilyDependencies #-}

module Data.Vect (
    Vect(..)
  , vlookup
  , toList
  , vLengthS
  , vExactLength
  , HVect(..)
  , hvlookup
  , VLookup
  , VLength
  , Sing(..)
  , svlookup
  , svlength
  , svExactLength
) where

import Data.Kind (Type)
import Data.Type.Equality

import Data.Nat
import Data.Sing

data Vect :: Nat -> Type -> Type where
  VNil :: Vect 'Z a
  (:>) :: a -> Vect n a -> Vect ('S n) a
infixr 5 :>

vlookup :: Fin n -> Vect n a -> a
vlookup FZ (x :> _) = x
vlookup (FS f) (_ :> xs) = vlookup f xs

toList :: Vect n a -> [a]
toList VNil = []
toList (x :> xs) = x:(toList xs)

-- behold the C U S K
data HVect :: forall (n :: Nat) . Vect n Type -> Type where
  HVNil :: HVect 'VNil
  (::>) :: a -> HVect as -> HVect (a ':> as)
infixr 5 ::>

hvlookup :: forall (n :: Nat) (as :: Vect n Type) (f :: Fin n)
          . Sing f -> HVect as -> VLookup f as
hvlookup SFZ (x ::> _) = x
hvlookup (SFS f) (_ ::> xs) = hvlookup f xs

type family VLookup (f :: Fin n) (as :: Vect n k) = (r :: k) where
  VLookup 'FZ (a ':> _) = a
  VLookup ('FS f) (_ ':> as) = VLookup f as

-- TODO: there's got to be an easier way to "reflect" n
type family VLength (as :: Vect n a) = (m :: Nat) | m -> n where
  VLength 'VNil = 'Z
  VLength (_ ':> as) = 'S (VLength as)

deriving instance Show a => Show (Vect n a)
deriving instance Eq a => Eq (Vect n a)
deriving instance Functor (Vect n)

instance Foldable (Vect n) where
  foldMap _ VNil = mempty
  foldMap f (x :> xs) = f x `mappend` foldMap f xs

vLengthS :: Vect n a -> Sing n
vLengthS VNil = SZ
vLengthS (_ :> xs) = SS (vLengthS xs)

vExactLength :: Sing m -> Vect n a -> Maybe (Vect m a)
vExactLength m v = case m of
  SZ -> case v of
    VNil -> Just VNil
    _ -> Nothing
  (SS k) -> case v of
    (x :> xs) -> (x :>) <$> vExactLength k xs
    _ -> Nothing

instance Show (HVect 'VNil) where
  show HVNil = "HVNil"
instance (Show a, Show (HVect as)) => Show (HVect (a ':> as)) where
  show (x ::> xs) = show x ++ " ::> " ++ show xs

instance Eq (HVect 'VNil) where
  HVNil == HVNil = True
instance (Eq a, Eq (HVect as)) => Eq (HVect (a ':> as)) where
  (x ::> xs) == (y ::> ys) = x == y && xs == ys

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

instance TestEquality (Sing :: k -> Type)
      => TestEquality (Sing :: Vect n k -> Type) where
  testEquality SVNil SVNil = Just Refl
  testEquality (x :-> xs) (y :-> ys)
    | Just Refl <- testEquality x y, Just Refl <- testEquality xs ys = Just Refl
  testEquality _ _ = Nothing

svlookup :: forall (n :: Nat) (as :: Vect n k) (f :: Fin n)
          . Sing f -> Sing as -> Sing (VLookup f as)
svlookup SFZ (x :-> _) = x
svlookup (SFS f) (_ :-> xs) = svlookup f xs

svlength :: forall (n :: Nat) (as :: Vect n k) . Sing as -> Sing (VLength as)
svlength SVNil = SZ
svlength (_ :-> xs) = SS (svlength xs)

svExactLength :: forall (m :: Nat) (n :: Nat) (a :: Type)
                        (as :: Vect m a) (as' :: Vect n a) .
                 (ISing as', SingKind a)
              => Sing n -> Sing as -> Maybe (Sing as')
svExactLength n sv = case vExactLength n (unsing sv) of
    Just _ -> Just sing
    _ -> Nothing

instance ShowSing a => ShowSing (Vect (n :: Nat) a) where
  showSing SVNil = "SVNil"
  showSing (x :-> xs) = showSing x ++ " :-> " ++ showSing xs

instance Eq (Sing 'VNil) where
  SVNil == SVNil = True
instance (Eq (Sing a), Eq (Sing as)) => Eq (Sing (a ':> as)) where
  (x :-> xs) == (y :-> ys) = x == y && xs == ys
