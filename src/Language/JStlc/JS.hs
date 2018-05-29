{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.JS (
    JS(..)
  , ToJS(..)
  , emit
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

class ToJS a where
  toJS :: a -> JS

instance ToJS Bool where
  toJS = JSBool

instance ToJS Int where
  toJS = JSNumber . fromIntegral

instance ToJS T.Text where
  toJS = JSString . escape where
    escape = T.replace "\"" "\\\""

instance ToJS a => ToJS (Maybe a) where
  toJS Nothing = JSVar "null"
  toJS (Just v) = toJS v

instance ToJS a => ToJS [a] where
  toJS = JSArray . fmap toJS

emit :: JS -> T.Text
emit (JSBool True) = "true"
emit (JSBool False) = "false"
emit (JSNumber x) = T.pack $ show x
emit (JSString s) = T.cons '\"' $ T.snoc s '\"'
emit (JSVar x) = x
emit (JSArray xs) = T.cons '[' $ T.snoc (T.intercalate ", " $ fmap emit xs) ']'
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
