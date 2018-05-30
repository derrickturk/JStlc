{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Language.JStlc.Syntax (
    Ty(..)
  , STy(..)
  , ISTy(..)
  , unSTy
  , ValTy
  , Ix(..)
  , BinOp(..)
  , Term(..)
) where

import Data.Type.Equality
import qualified Data.Text as T

import Language.JStlc.JS

-- TODO: could be a lark to use a Vect for the type context
{--
data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
--}
-- implementor's note: it was not, in fact, a lark

data Ty :: * where
  IntTy :: Ty
  BoolTy :: Ty
  StringTy :: Ty
  FnTy :: Ty -> Ty -> Ty
  OptionTy :: Ty -> Ty
  ListTy :: Ty -> Ty

data STy :: Ty -> * where
  SIntTy :: STy 'IntTy 
  SBoolTy :: STy 'BoolTy 
  SStringTy :: STy 'StringTy 
  SFnTy :: STy a -> STy b -> STy ('FnTy a b)
  SOptionTy :: STy a -> STy ('OptionTy a)
  SListTy :: STy a -> STy ('ListTy a)

class ISTy a where
  sTy :: STy a
instance ISTy 'IntTy where
  sTy = SIntTy
instance ISTy 'BoolTy where
  sTy = SBoolTy
instance ISTy 'StringTy where
  sTy = SStringTy
instance (ISTy a, ISTy b) => ISTy ('FnTy a b) where
  sTy = SFnTy sTy sTy
instance ISTy a => ISTy ('OptionTy a) where
  sTy = SOptionTy sTy
instance ISTy a => ISTy ('ListTy a) where
  sTy = SListTy sTy

unSTy :: STy a -> Ty
unSTy SIntTy = IntTy
unSTy SBoolTy = BoolTy
unSTy SStringTy = StringTy
unSTy (SFnTy a b) = FnTy (unSTy a) (unSTy b)
unSTy (SOptionTy a) = OptionTy (unSTy a)
unSTy (SListTy a) = ListTy (unSTy a)

instance TestEquality STy where
  testEquality SIntTy SIntTy = Just Refl
  testEquality SBoolTy SBoolTy = Just Refl
  testEquality SStringTy SStringTy = Just Refl
  testEquality (SFnTy a b) (SFnTy c d)
    | Just Refl <- testEquality a c, Just Refl <- testEquality b d = Just Refl
  testEquality (SOptionTy a) (SOptionTy b)
    | Just Refl <- testEquality a b = Just Refl
  testEquality (SListTy a) (SListTy b)
    | Just Refl <- testEquality a b = Just Refl
  testEquality _ _ = Nothing

type family ValTy (a :: Ty) = v | v -> a where
  ValTy 'IntTy = Int
  ValTy 'BoolTy = Bool
  ValTy 'StringTy = T.Text
  ValTy ('FnTy a b) = ValTy a -> ValTy b
  ValTy ('OptionTy a) = Maybe (ValTy a)
  ValTy ('ListTy a) = [ValTy a]

data Ix :: [Ty] -> Ty -> * where
  IZ :: Ix (a ': as) a
  IS :: Ix as a -> Ix (b ': as) a

data BinOp :: Ty -> Ty -> * where
  Add :: BinOp 'IntTy 'IntTy
  Sub :: BinOp 'IntTy 'IntTy
  Mul :: BinOp 'IntTy 'IntTy
  Div :: BinOp 'IntTy 'IntTy
  Or :: BinOp 'BoolTy 'BoolTy
  And :: BinOp 'BoolTy 'BoolTy
  StrCat :: BinOp 'StringTy 'StringTy
  Append :: BinOp ('ListTy a) ('ListTy a)
  Eq :: Eq (ValTy a) => BinOp a 'BoolTy

data Term :: [Ty] -> Ty -> * where
  Var :: Ix ts a -> Term ts a
  Lit :: (Show (ValTy a), ToJS (ValTy a)) => ValTy a -> Term ts a
  Lam :: T.Text -> STy a -> Term (a ': ts) b -> Term ts ('FnTy a b)
  App :: Term ts ('FnTy a b) -> Term ts a -> Term ts b
  None :: STy ('OptionTy a) -> Term ts ('OptionTy a)
  Some :: Term ts a -> Term ts ('OptionTy a)
  Nil :: STy ('ListTy a) -> Term ts ('ListTy a)
  Cons :: Term ts a -> Term ts ('ListTy a) -> Term ts ('ListTy a)
  BinOpApp :: BinOp a b -> Term ts a -> Term ts a -> Term ts b
  IfThenElse :: Term ts 'BoolTy -> Term ts a -> Term ts a -> Term ts a
  FoldL :: Term ts ('FnTy a ('FnTy b a))
        -> Term ts a
        -> Term ts ('ListTy b)
        -> Term ts a
  MapOption :: Term ts ('FnTy a b)
            -> Term ts ('OptionTy a)
            -> Term ts ('OptionTy b)
  MapList :: Term ts ('FnTy a b)
          -> Term ts ('ListTy a)
          -> Term ts ('ListTy b)

instance Show Ty where
  show IntTy = "IntTy"
  show BoolTy = "BoolTy"
  show StringTy = "StringTy"
  show (FnTy a b) = "FnTy (" ++ show a ++ ") (" ++ show b ++ ")"
  show (OptionTy a) = "OptionTy (" ++ show a ++ ")"
  show (ListTy a) = "OptionTy (" ++ show a ++ ")"

instance Show (STy a) where
  show SIntTy = "SIntTy"
  show SBoolTy = "SBoolTy"
  show SStringTy = "SStringTy"
  show (SFnTy a b) = "SFnTy (" ++ show a ++ ") (" ++ show b ++ ")"
  show (SOptionTy a) = "SOptionTy (" ++ show a ++ ")"
  show (SListTy a) = "SOptionTy (" ++ show a ++ ")"

instance Show (Ix as a) where
  show = show . toInt where
    toInt :: Ix as a -> Int
    toInt IZ = 0
    toInt (IS i) = 1 + toInt i

instance Show (BinOp a b) where
  show Add = "Add"
  show Sub = "Sub"
  show Mul = "Mul"
  show Div = "Div"
  show Or = "Or"
  show And = "And"
  show StrCat = "StrCat"
  show Append = "Append"
  show Eq = "Eq"

instance Show (Term as a) where
  show (Var i) = "Var " ++ show i
  show (Lit v) = show v
  show (Lam x ty body) =
    "Lam " ++ show x ++ " " ++ show ty ++ " (" ++ show body ++ ")"
  show (App x y) = "App (" ++ show x ++ ") (" ++ show y ++ ")"
  show (None ty) = "None " ++ show ty
  show (Some x) = "Some (" ++ show x ++ ")"
  show (Nil ty) = "Nil " ++ show ty
  show (Cons x xs) = "Cons (" ++ show x ++ ") (" ++ show xs ++ ")"
  show (BinOpApp op x y) =
    "BinOpApp " ++ show op ++ "(" ++ show x ++ ") (" ++ show y ++ ")"
  show (IfThenElse cond t f) =
    "IfThenElse (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show f ++ ")"
  show (FoldL f x xs) =
    "FoldL (" ++ show f ++ ") (" ++ show x ++ ") (" ++ show xs ++ ")"
  show (MapOption f x) = "MapOption (" ++ show f ++ ") (" ++ show x ++ ")"
  show (MapList f x) = "MapList (" ++ show f ++ ") (" ++ show x ++ ")"
