{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Check (
    TypeError(..)
  , ExTerm
  , runExTerm
  , ExStmt
  , runExStmt
  , ExProg
  , runExProg
  , ExNamedStmt
  , runExNamedStmt
  , ExNamedProg
  , runExNamedProg
  , check
  , check'
  , checkStmt
  , checkStmt'
  , checkProg
  , checkProg'
) where

import Data.Kind (Type)
import Data.Type.Equality
import Data.Monoid ((<>))
import qualified Data.Text as T

import Data.Nat
import Data.Vect
import Data.Sing

import Language.JStlc.Types
import Language.JStlc.Unchecked
import Language.JStlc.Syntax
import Language.JStlc.Pretty.Class

data TypeError :: Type where
  Mismatch :: Ty -> Ty -> TypeError
  UndefinedVar :: T.Text -> TypeError
  ExpectedFnType :: Ty -> TypeError
  ExpectedOptionType :: Ty -> TypeError
  ExpectedListType :: Ty -> TypeError
  ExpectedEqType :: Ty -> TypeError
  DuplicateDef :: T.Text -> TypeError

-- this MUST be a newtype, because -XImpredicativeTypes blows up on
--   the type alias version
newtype ExTerm ctxt =
  ExTerm { runExTerm :: forall r . (forall a . STy a -> Term ctxt a -> r) -> r }

newtype ExStmt before = ExStmt { runExStmt ::
  forall r . (forall (m :: Nat) (after :: TyCtxt m) .
              STyCtxt after -> Stmt before after -> r)
          -> r
  }

newtype ExProg = ExProg { runExProg ::
  forall r . (forall (n :: Nat) (as :: TyCtxt n) .
              STyCtxt as -> Prog as -> r)
          -> r
  }

-- required for checking stmts
-- they MUST share an existential context (to make the lengths unify)
-- also, the length n must be a parameter to the newtype
newtype ExNamedStmt before n = ExNamedStmt { runExNamedStmt ::
  forall r . (forall (after :: TyCtxt n) .
              STyCtxt after -> NameCtxt after -> Stmt before after -> r)
           -> r
  }

-- the same holds here
newtype ExNamedProg n = ExNamedProg { runExNamedProg ::
  forall r . (forall (as :: TyCtxt n) .
              STyCtxt as -> NameCtxt as -> Prog as -> r)
          -> r
  }

defined :: T.Text -> Vect n T.Text -> Bool
defined _ VNil = False
defined x (n :> ns) = if x == n then True else defined x ns

check :: UTerm 'Z -> Either TypeError (ExTerm 'VNil)
check = check' VNil SVNil

check' :: forall (n :: Nat) (as :: TyCtxt n) . NameCtxt as -> STyCtxt as -> UTerm n -> Either TypeError (ExTerm as)

check' n c (UVar x) = case varIx n c x of
  Just exX -> runExIx exX $
    \sX i -> Right $ ExTerm (\k -> k sX (Var i))
  Nothing -> Left $ UndefinedVar x

check' _ _ (ULit v) = Right $ ExTerm (\k -> k sing (Lit v))

check' n c (ULam x ty body) = do
  exBody <- check' (x :> n) (ty :-> c) body
  runExTerm exBody $
    \sBody tBody -> Right $ ExTerm (\k -> k (SFnTy ty sBody) (Lam x ty tBody))

check' n c (UApp f x) = do
  exF <- check' n c f
  exX <- check' n c x
  runExTerm exF $
    \sF tF -> case sF of
      SFnTy sA sB -> runExTerm exX $
        \sX tX -> case testEquality sX sA of
          Just Refl -> Right $ ExTerm (\k -> k sB (App tF tX))
          _ -> Left $ Mismatch (unsing sA) (unsing sX)
      _ -> Left $ ExpectedFnType (unsing sF)

check' n c (ULet x t u) = do
  exT <- check' n c t
  runExTerm exT $
    \sT tT -> do
      exU <- check' (x :> n) (sT :-> c) u
      runExTerm exU $
        \sU tU -> Right $ ExTerm (\k -> k sU (Let x tT tU))

check' n c (ULetRec x ty t u) = do
  exT <- check' (x :> n) (ty :-> c) t
  runExTerm exT $
    \sT tT -> case testEquality ty sT of
        Just Refl -> do
          exU <- check' (x :> n) (ty :-> c) u
          runExTerm exU $
            \sU tU -> Right $ ExTerm (\k -> k sU (LetRec x ty tT tU))
        _ -> Left $ Mismatch (unsing ty) (unsing sT)

check' n c (UFix x) = do
  exX <- check' n c x
  runExTerm exX $
    \s t -> case s of
      SFnTy sA sB -> case testEquality sA sB of
        Just Refl -> Right (ExTerm (\k -> k sA (Fix t)))
        _ -> Left $ Mismatch (FnTy (unsing sA) (unsing sA)) (unsing s)
      _ -> Left $ ExpectedFnType (unsing s)

check' _ _ (UNone ty@(SOptionTy _)) = Right $ ExTerm (\k -> k ty (None ty))
check' _ _ (UNone ty) = Left $ ExpectedOptionType (unsing ty)

check' n c (USome x) = do
  exX <- check' n c x
  runExTerm exX $
    \s t -> Right (ExTerm (\k -> k (SOptionTy s) (Some t)))

check' _ _ (UNil ty@(SListTy _)) = Right $ ExTerm (\k -> k ty (Nil ty))
check' _ _ (UNil ty) = Left $ ExpectedListType (unsing ty)

check' n c (UCons x xs) = do
  exX <- check' n c x
  exXs <- check' n c xs
  runExTerm exX $
    \s1 t1 -> runExTerm exXs $
      \s2 t2 -> case s2 of
        SListTy s2' -> case testEquality s1 s2' of
          Just Refl -> Right $ ExTerm (\k -> k (SListTy s2') (Cons t1 t2))
          _ -> Left $ Mismatch (unsing s2') (unsing s1)
        _ -> Left $ ExpectedListType (unsing s2)

check' n c (UBinOpApp op x y) = do
  exX <- check' n c x
  exY <- check' n c y
  exOp <- checkBinOp op exX exY
  runExBinOp exOp $
    \aTy resTy op' -> runExTerm exX $
      \s1 t1 -> case testEquality s1 aTy of
        Just Refl -> runExTerm exY $
          \s2 t2 -> case testEquality s2 aTy of
            Just Refl -> Right $ ExTerm (\k -> k resTy (BinOpApp op' t1 t2))
            _ -> Left $ Mismatch (unsing aTy) (unsing s2)
        _ -> Left $ Mismatch (unsing aTy) (unsing s1)

check' n c (UIfThenElse b t f) = do
  exB <- check' n c b
  exT <- check' n c t
  exF <- check' n c f
  runExTerm exB $
    \sB tB -> case testEquality sB SBoolTy of
      Just Refl -> runExTerm exT $
        \sT tT -> runExTerm exF $
          \sF tF -> case testEquality sT sF of
            Just Refl -> Right $ ExTerm (\k -> k sT (IfThenElse tB tT tF))
            _ -> Left $ Mismatch (unsing sT) (unsing sF)
      _ -> Left $ Mismatch BoolTy (unsing sB)

check' n c (UFoldL f x xs) = do
  exF <- check' n c f
  exX <- check' n c x
  exXs <- check' n c xs
  runExTerm exF $
    \sF tF -> case sF of
      SFnTy sA sBA -> case sBA of
        SFnTy sB sA' -> case testEquality sA' sA of
          Just Refl -> runExTerm exX $
            \sX tX -> case testEquality sX sA of
              Just Refl -> runExTerm exXs $
                \sXs tXs -> case sXs of
                  SListTy sElem -> case testEquality sElem sB of
                    Just Refl -> Right $ ExTerm (\k -> k sA (FoldL tF tX tXs)) 
                    _ -> Left $ Mismatch (ListTy (unsing sB)) (unsing sXs)
                  _ -> Left $ Mismatch (ListTy (unsing sB)) (unsing sXs)
              _ -> Left $ Mismatch (unsing sA) (unsing sX)
          _ -> Left $ Mismatch (FnTy (unsing sB) (unsing sA)) (unsing sBA)
        _ -> Left $ ExpectedFnType (unsing sBA)
      _ -> Left $ ExpectedFnType (unsing sF)

check' n c (UMap f x) = do
  exF <- check' n c f
  exX <- check' n c x
  runExTerm exF $
    \sF tF -> case sF of
      SFnTy sA sB -> runExTerm exX $
        \sX tX -> case sX of
          SOptionTy sX' -> case testEquality sX' sA of
            Just Refl -> Right $
              ExTerm (\k -> k (SOptionTy sB) (MapOption tF tX)) 
            _ -> Left $ Mismatch (OptionTy (unsing sA)) (unsing sX)
          SListTy sX' -> case testEquality sX' sA of
            Just Refl -> Right $
              ExTerm (\k -> k (SListTy sB) (MapList tF tX)) 
            _ -> Left $ Mismatch (ListTy (unsing sA)) (unsing sX)
          -- this is a bit arbitrary
          _ -> Left $ ExpectedListType (unsing sX)
      _ -> Left $ ExpectedFnType (unsing sF)

newtype ExIx ctxt =
  ExIx { runExIx :: forall r . (forall a . STy a -> Ix ctxt a -> r) -> r }

varIx :: NameCtxt as -> STyCtxt as -> T.Text -> Maybe (ExIx as)
varIx VNil _ _ = Nothing 
-- varIx _ SVNil _ = Nothing / this case is statically detected as impossible!
varIx (x :> xs) (ty :-> tys) name = if name == x
  then Just $ ExIx (\k -> k ty IZ)
  else do
    exIx <- varIx xs tys name
    runExIx exIx $ \s i -> Just $ ExIx (\k -> k s (IS i)) 

newtype ExBinOp =
  ExBinOp { runExBinOp :: forall r .
                          (forall a b . STy a -> STy b -> BinOp a b -> r)
                       -> r
          }

newtype ExEqValTy =
  ExEqValTy { runExEqValTy :: forall r .
                              (forall a . Eq (ValTy a) => STy a -> r)
                           -> r
            }

-- TODO: figure out how to generalize this to other "polymorphic" constraints
testEqValTy :: STy a -> Maybe ExEqValTy
testEqValTy SIntTy = Just $ ExEqValTy ($ SIntTy)
testEqValTy SBoolTy = Just $ ExEqValTy ($ SBoolTy)
testEqValTy SStringTy = Just $ ExEqValTy ($ SStringTy)
testEqValTy (SFnTy _ _) = Nothing
testEqValTy (SOptionTy a) = do
  exEqA <- testEqValTy a
  runExEqValTy exEqA $ \s -> Just $ ExEqValTy (\k -> k (SOptionTy s))
testEqValTy (SListTy a) = do
  exEqA <- testEqValTy a
  runExEqValTy exEqA $ \s -> Just $ ExEqValTy (\k -> k (SListTy s))

-- TODO: this is a weird way to split this functionality out
-- this function only checks "polymorphism conditions" on the operators
-- leaving the main check' function to handle operand checking
checkBinOp :: UBinOp -> ExTerm as -> ExTerm as -> Either TypeError ExBinOp
checkBinOp UAdd _ _ = Right $ ExBinOp (\k -> k SIntTy SIntTy Add)
checkBinOp USub _ _ = Right $ ExBinOp (\k -> k SIntTy SIntTy Sub)
checkBinOp UMul _ _ = Right $ ExBinOp (\k -> k SIntTy SIntTy Mul)
checkBinOp UDiv _ _ = Right $ ExBinOp (\k -> k SIntTy SIntTy Div)
checkBinOp UMod _ _ = Right $ ExBinOp (\k -> k SIntTy SIntTy Mod)
checkBinOp UOr _ _ = Right $ ExBinOp (\k -> k SBoolTy SBoolTy Or)
checkBinOp UAnd _ _ = Right $ ExBinOp (\k -> k SBoolTy SBoolTy And)
checkBinOp ULt _ _ = Right $ ExBinOp (\k -> k SIntTy SBoolTy Lt)
checkBinOp ULtEq _ _ = Right $ ExBinOp (\k -> k SIntTy SBoolTy LtEq)
checkBinOp UGt _ _ = Right $ ExBinOp (\k -> k SIntTy SBoolTy Gt)
checkBinOp UGtEq _ _ = Right $ ExBinOp (\k -> k SIntTy SBoolTy GtEq)
checkBinOp UStrCat _ _ = Right $ ExBinOp (\k -> k SStringTy SStringTy StrCat)
checkBinOp UAppend exX _ = runExTerm exX $ \sX _ -> case sX of
  SListTy a -> Right $ ExBinOp (\k -> k (SListTy a) (SListTy a) Append)
  _ -> Left $ ExpectedListType (unsing sX)
checkBinOp UEq exX _ = runExTerm exX $
  \sX _ -> case testEqValTy sX of
    Just exEqX -> runExEqValTy exEqX $
      \s -> Right $ ExBinOp (\k -> k s SBoolTy Eq)
    Nothing -> Left $ ExpectedEqType (unsing sX)

checkStmt :: forall (m :: Nat) (before :: TyCtxt m) (n :: Nat)
           . NameCtxt before
          -> STyCtxt before
          -> UStmt m n
          -> Either TypeError (ExStmt before)
checkStmt n c s = do
  exNS <- checkStmt' n c s
  runExNamedStmt exNS $ \sC _ sS -> Right $ ExStmt (\k -> k sC sS)

-- TODO: enforce uniqueness of names in types

checkStmt' :: forall (m :: Nat) (before :: TyCtxt m) (n :: Nat)
            . NameCtxt before
           -> STyCtxt before
           -> UStmt m n
           -> Either TypeError (ExNamedStmt before n)
checkStmt' n c (UDefine x t) = if defined x n
  then Left $ DuplicateDef x
  else do
    exT <- check' n c t
    runExTerm exT $
      \s t' -> Right $ ExNamedStmt (\k -> k (s :-> c) (x :> n) (Define x t'))

checkStmt' n c (UDefineTyped x ty t) = if defined x n
  then Left $ DuplicateDef x
  else do
    exT <- check' n c t
    runExTerm exT $ \s t' -> case testEquality s ty of
      Just Refl -> Right $
        ExNamedStmt (\k -> k (s :-> c) (x :> n) (Define x t'))
      _ -> Left $ Mismatch (unsing ty) (unsing s)

checkStmt' n c (UDefineRec x ty t) = if defined x n
  then Left $ DuplicateDef x
  else do
    exT <- check' (x :> n) (ty :-> c) t
    runExTerm exT $ \s t' -> case testEquality s ty of
      Just Refl ->
        Right $ ExNamedStmt (\k -> k (s :-> c) (x :> n) (DefineRec x ty t'))
      _ -> Left $ Mismatch (unsing ty) (unsing s)

checkProg :: UProg n -> Either TypeError ExProg
checkProg p = checkProg' p >>= \exNP ->
  runExNamedProg exNP $ \pC _ pP -> Right $ ExProg (\k -> k pC pP)

checkProg' :: UProg n -> Either TypeError (ExNamedProg n)
checkProg' UEmptyProg = Right $ ExNamedProg (\k -> k SVNil VNil EmptyProg)
checkProg' (p :&?: s) = do
  exNP <- checkProg' p
  runExNamedProg exNP $ \pC pN pP -> do
    exNS <- checkStmt' pN pC s
    runExNamedStmt exNS $ \sC sN sS -> Right $
      ExNamedProg $ \k -> k sC sN (pP :&: sS)

instance Show TypeError where
  show (Mismatch e f) = "Mismatch (" ++ show e ++ ") (" ++ show f ++ ")"
  show (UndefinedVar x) = "UndefinedVar " ++ show x
  show (ExpectedFnType ty) = "ExpectedFnType (" ++ show ty ++ ")"
  show (ExpectedOptionType ty) = "ExpectedOptionType (" ++ show ty ++ ")"
  show (ExpectedListType ty) = "ExpectedListType (" ++ show ty ++ ")"
  show (ExpectedEqType ty) = "ExpectedEqType (" ++ show ty ++ ")"
  show (DuplicateDef x) = "DuplicateDef " ++ show x

instance Show (ExTerm ctxt) where
  show exT = runExTerm exT $ \_ t -> show t

instance Show (ExStmt before) where
  show exS = runExStmt exS $ \_ s -> show s

instance Show ExProg where
  show exP = runExProg exP $ \_ p -> show p

instance Pretty TypeError where
  pretty (Mismatch ex found) = "Type error: expected " <>
    pretty ex <> "; found " <> pretty found
  pretty (UndefinedVar x) = "Undefined variable: " <> x
  pretty (ExpectedFnType found) =
    "Type error: expected function type; found " <> pretty found
  pretty (ExpectedOptionType found) =
    "Type error: expected option type; found " <> pretty found
  pretty (ExpectedListType found) =
    "Type error: expected list type; found " <> pretty found
  pretty (ExpectedEqType found) =
    "Type error: expected equality-supporting type; found " <> pretty found
  pretty (DuplicateDef x) = "Multiple defintions for " <> x
