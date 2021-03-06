{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, TypeInType #-}

module Language.JStlc.Syntax (
    Ix(..)
  , toFin
  , BinOp(..)
  , Term(..)
  , Stmt(..)
  , Prog(..)
) where

import Data.Kind (Type)
import qualified Data.Text as T

import Data.Nat
import Data.Vect

import Language.JStlc.Types
import Language.JStlc.JS

-- TODO: unary ops (not, negate)

data Ix :: forall (n :: Nat) a . Vect n a -> a -> Type where
  IZ :: Ix (a ':> as) a
  IS :: Ix as a -> Ix (b ':> as) a

toFin :: Ix as a -> Fin (VLength as)
toFin IZ = FZ
toFin (IS i) = FS (toFin i)

data BinOp :: Ty -> Ty -> Type where
  Add :: BinOp 'IntTy 'IntTy
  Sub :: BinOp 'IntTy 'IntTy
  Mul :: BinOp 'IntTy 'IntTy
  Div :: BinOp 'IntTy 'IntTy
  Mod :: BinOp 'IntTy 'IntTy
  Or :: BinOp 'BoolTy 'BoolTy
  And :: BinOp 'BoolTy 'BoolTy
  Lt :: BinOp 'IntTy 'BoolTy
  LtEq :: BinOp 'IntTy 'BoolTy
  Gt :: BinOp 'IntTy 'BoolTy
  GtEq :: BinOp 'IntTy 'BoolTy
  StrCat :: BinOp 'StringTy 'StringTy
  Append :: BinOp ('ListTy a) ('ListTy a)
  Eq :: Eq (ValTy a) => BinOp a 'BoolTy

data Term :: forall (n :: Nat) . TyCtxt n -> Ty -> Type where
  Var :: Ix ts a -> Term ts a
  Lit :: (Show (ValTy a), ToJS (ValTy a)) => ValTy a -> Term ts a
  Lam :: T.Text -> STy a -> Term (a ':> ts) b -> Term ts ('FnTy a b)
  App :: Term ts ('FnTy a b) -> Term ts a -> Term ts b
  Let :: T.Text -> Term ts a -> Term (a ':> ts) b -> Term ts b
  LetRec :: T.Text
         -> STy a
         -> Term (a ':> ts) a
         -> Term (a ':> ts) b
         -> Term ts b
  Fix :: Term ts ('FnTy a a) -> Term ts a
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
  Head :: Term ts ('ListTy a) -> Term ts ('OptionTy a)
  Tail :: Term ts ('ListTy a) -> Term ts ('OptionTy ('ListTy a))

-- statements are indexed by their "before" and "after" type contexts
data Stmt :: forall (n :: Nat) (m :: Nat) . TyCtxt n -> TyCtxt m -> Type where
  Define :: T.Text -> Term as a -> Stmt as (a ':> as)
  DefineRec :: T.Text -> STy a -> Term (a ':> as) a -> Stmt as (a ':> as)

-- TODO: :&: could require a proof of name uniqueness
data Prog :: forall (n :: Nat) . TyCtxt n -> Type where
  EmptyProg :: Prog 'VNil
  (:&:) :: Prog before -> Stmt before after -> Prog after
infixr 5 :&:

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
  show Mod = "Mod"
  show Or = "Or"
  show And = "And"
  show Lt = "Lt"
  show LtEq = "LtEq"
  show Gt = "Gt"
  show GtEq = "GtEq"
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
    "Let " ++ show x ++ " (" ++ show t ++ ") (" ++ show u ++ ")"
  show (LetRec x ty t u) =
    "LetRec " ++ show x ++ " " ++ show ty ++
    " (" ++ show t ++ ") (" ++ show u ++ ")"
  show (Fix t) = "Fix (" ++ show t ++ ")"
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
  show (Head xs) = "Head (" ++ show xs ++ ")"
  show (Tail xs) = "Tail (" ++ show xs ++ ")"

instance Show (Stmt before after) where
  show (Define x t) = "Define " ++ show x ++ " (" ++ show t ++ ")"
  show (DefineRec x ty t) =
    "DefineRec " ++ show x ++ " " ++ show ty ++ " (" ++ show t ++ ")"

instance Show (Prog as) where
  show EmptyProg = "EmptyProg"
  show (p :&: s) = show p ++ " :&: " ++ show s
