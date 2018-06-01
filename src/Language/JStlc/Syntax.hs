{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Language.JStlc.Syntax (
    Ix(..)
  , BinOp(..)
  , Term(..)
) where

import qualified Data.Text as T

import Language.JStlc.Types
import Language.JStlc.JS

-- TODO: could be a lark to use a Vect for the type context
-- implementor's note: it was not, in fact, a lark

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
  Let :: T.Text -> Term ts a -> Term (a ': ts) b -> Term ts b
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
  show (Lit v) = "Lit " ++ show v
  show (Lam x ty body) =
    "Lam " ++ show x ++ " " ++ show ty ++ " (" ++ show body ++ ")"
  show (App x y) = "App (" ++ show x ++ ") (" ++ show y ++ ")"
  show (Let x t u) =
    "Let " ++ show x ++ ") (" ++ show t ++ ") (" ++ show u ++ ")"
  show (None ty) = "None " ++ show ty
  show (Some x) = "Some (" ++ show x ++ ")"
  show (Nil ty) = "Nil " ++ show ty
  show (Cons x xs) = "Cons (" ++ show x ++ ") (" ++ show xs ++ ")"
  show (BinOpApp op x y) =
    "BinOpApp " ++ show op ++ " (" ++ show x ++ ") (" ++ show y ++ ")"
  show (IfThenElse cond t f) =
    "IfThenElse (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show f ++ ")"
  show (FoldL f x xs) =
    "FoldL (" ++ show f ++ ") (" ++ show x ++ ") (" ++ show xs ++ ")"
  show (MapOption f x) = "MapOption (" ++ show f ++ ") (" ++ show x ++ ")"
  show (MapList f x) = "MapList (" ++ show f ++ ") (" ++ show x ++ ")"
