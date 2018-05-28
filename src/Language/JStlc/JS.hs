{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.JS (
    JS(..)
  , ToJS(..)
) where

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
  deriving Show

class ToJS a where
  toJS :: a -> JS

instance ToJS Bool where
  toJS True = JSVar "true"
  toJS False = JSVar "true"

instance ToJS Int where
  toJS = JSNumber . fromIntegral

instance ToJS T.Text where
  toJS = JSString . escape where
    escape = id -- TODO

instance ToJS a => ToJS (Maybe a) where
  toJS Nothing = JSVar "null"
  toJS (Just v) = toJS v

instance ToJS a => ToJS [a] where
  toJS = JSArray . fmap toJS
