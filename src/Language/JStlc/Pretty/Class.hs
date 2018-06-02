{-# LANGUAGE DefaultSignatures #-}

module Language.JStlc.Pretty.Class (
    Pretty(..)
) where

import qualified Data.Text as T

class Pretty a where
  pretty :: a -> T.Text
  default pretty :: Show a => a -> T.Text
  pretty = T.pack . show
