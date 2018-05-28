module Language.JStlc.Internal (
    LitString(..)
) where

newtype LitString = LitString { toString :: String }
