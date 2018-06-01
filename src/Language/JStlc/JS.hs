{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.JS (
    JS(..)
  , JSStmt(..)
  , JSProg
  , ToJS(..)
  , Emit(..)
) where

import Data.Monoid ((<>))
import qualified Data.Text as T

data JS =
    JSBool Bool
  | JSNumber Double
  | JSString T.Text
  | JSVar T.Text
  | JSArray [JS]
  | JSLambda T.Text JS
  | JSCall JS [JS]
  | JSMethod JS T.Text [JS]
  | JSCondExpr JS JS JS
  | JSUnOpApp T.Text JS
  | JSBinOpApp T.Text JS JS
  deriving Show

data JSStmt =
    JSLet T.Text JS
  | JSFunDef T.Text [T.Text] JS
    deriving Show

type JSProg = [JSStmt]

class ToJS a where
  toJS :: a -> JS

instance ToJS Bool where
  toJS = JSBool

instance ToJS Integer where
  toJS = JSNumber . fromIntegral

instance ToJS T.Text where
  toJS = JSString . escape where
    escape = T.replace "\"" "\\\""

instance ToJS a => ToJS (Maybe a) where
  toJS Nothing = JSVar "null"
  toJS (Just v) = toJS v

instance ToJS a => ToJS [a] where
  toJS = JSArray . fmap toJS

class Emit a where
  emit :: a -> T.Text

instance Emit JS where
  emit (JSBool True) = "true"
  emit (JSBool False) = "false"
  emit (JSNumber x) = T.pack $ show x
  emit (JSString s) = T.cons '\"' $ T.snoc s '\"'
  emit (JSVar x) = x
  emit (JSArray xs) =
    T.cons '[' $ T.snoc (T.intercalate ", " $ fmap emit xs) ']'
  emit (JSLambda x body) =
    "function (" <> x <> ") { return " <> emit body <> "; }"
  emit (JSCall func args) =
    "(" <> emit func <> ")(" <> T.intercalate ", " (fmap emit args) <> ")"
  emit (JSMethod obj func args) =
    "(" <> emit obj <> ")."<> func <> "("
    <> T.intercalate ", " (fmap emit args) <> ")"
  emit (JSCondExpr cond t f) =
    "(" <> emit cond <> ") ? (" <> emit t <> ") : (" <> emit f <> ")"
  emit (JSUnOpApp op x) = op <> "(" <> emit x <> ")"
  emit (JSBinOpApp op x y) = "(" <> emit x <> ") " <> op <> " (" <> emit y <> ")"

instance Emit JSStmt where
  emit (JSLet x t) = "var " <> x <> " = " <> emit t <> ";\n"
  emit (JSFunDef f args body) =
    "function " <> f <> "(" <> T.intercalate ", " args <> ") {\n\treturn "
    <> emit body <> ";\n}\n"
