{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.JS (
    JS(..)
  , JSStmt(..)
  , JSProg(..)
  , ToJS(..)
  , Emit(..)
) where

import Data.Monoid ((<>))
import Data.Foldable (toList)
import qualified Data.Text as T

import Data.Nat
import Data.Vect hiding (toList)

data JS :: Nat -> * where
  JSBool :: Bool -> JS n
  JSNumber :: Double -> JS n
  JSInteger :: Integer -> JS n
  JSString :: T.Text -> JS n
  JSBuiltIn :: T.Text -> JS n
  JSVar :: Fin n -> T.Text -> JS n
  JSArray :: [JS n] -> JS n
  JSLambda :: T.Text -> JS ('S n) -> JS n
  JSCall :: JS n -> [JS n] -> JS n
  JSMethod :: JS n -> T.Text -> [JS n] -> JS n
  JSProperty :: JS n -> T.Text -> JS n
  JSCondExpr :: JS n -> JS n -> JS n -> JS n
  JSUnOpApp :: T.Text -> JS n -> JS n
  JSBinOpApp :: T.Text -> JS n -> JS n -> JS n
  JSIndex :: JS n -> JS n -> JS n
  deriving Show

data JSStmt :: Nat -> Nat -> * where
  JSLet :: T.Text -> JS n -> JSStmt n ('S n)
  JSFunDef :: T.Text -> Vect m T.Text -> JS (n :+: m) -> JSStmt n ('S n)
  -- TODO: find a way to unify this with JSFunDef
  -- (likely by shifting... somewhere)
  JSFunDefRec :: T.Text -> Vect m T.Text -> JS ('S (n :+: m)) -> JSStmt n ('S n)

data JSProg :: Nat -> * where
  JSEmptyProg :: JSProg 'Z
  (:&&:) :: JSProg m -> JSStmt m n -> JSProg n
infixr 5 :&&:

class ToJS a where
  toJS :: a -> JS n

instance ToJS Bool where
  toJS = JSBool

instance ToJS Integer where
  toJS = JSInteger

instance ToJS T.Text where
  toJS = JSString . escape where
    escape = T.replace "\"" "\\\""

instance ToJS a => ToJS (Maybe a) where
  toJS Nothing = JSBuiltIn "null"
  toJS (Just v) = toJS v

instance ToJS a => ToJS [a] where
  toJS = JSArray . fmap toJS

class Emit a where
  emit :: a -> T.Text

instance Emit (JS n) where
  emit (JSBool True) = "true"
  emit (JSBool False) = "false"
  emit (JSNumber x) = T.pack $ show x
  emit (JSInteger x) = T.pack $ show x
  emit (JSString s) = T.cons '\"' $ T.snoc s '\"'
  emit (JSBuiltIn x) = x
  emit (JSVar _ x) = x
  emit (JSArray xs) =
    T.cons '[' $ T.snoc (T.intercalate ", " $ fmap emit xs) ']'
  emit (JSLambda x body) =
    "function (" <> x <> ") { return " <> emit body <> "; }"
  emit (JSCall func args) =
    "(" <> emit func <> ")(" <> T.intercalate ", " (fmap emit args) <> ")"
  emit (JSMethod obj func args) =
    "(" <> emit obj <> ")."<> func <> "("
    <> T.intercalate ", " (fmap emit args) <> ")"
  emit (JSProperty obj prop) = "(" <> emit obj <> ")."<> prop
  emit (JSCondExpr cond t f) =
    "(" <> emit cond <> ") ? (" <> emit t <> ") : (" <> emit f <> ")"
  emit (JSUnOpApp op x) = op <> "(" <> emit x <> ")"
  emit (JSBinOpApp op x y) = "(" <> emit x <> ") " <> op <> " (" <> emit y <> ")"
  emit (JSIndex x i) = "(" <> emit x <> ")[" <> emit i <> "]"

instance Emit (JSStmt n m) where
  emit (JSLet x t) = "var " <> x <> " = " <> emit t <> ";\n"
  emit (JSFunDef f args body) =
    "function " <> f <> "(" <> T.intercalate ", " (toList args) <> ") {\n\treturn "
    <> emit body <> ";\n}\n"
  emit (JSFunDefRec f args body) =
    "function " <> f <> "(" <> T.intercalate ", " (toList args) <> ") {\n\treturn "
    <> emit body <> ";\n}\n"

instance Emit (JSProg n) where
  emit = T.intercalate "\n" . go [] where
    go :: [T.Text] -> JSProg n -> [T.Text]
    go stmts JSEmptyProg = stmts
    go stmts (p :&&: s) = go ((emit s):stmts) p

deriving instance Show (JSStmt m n)
deriving instance Show (JSProg n)
