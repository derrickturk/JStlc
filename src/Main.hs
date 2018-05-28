{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Language.JStlc.Syntax
import Language.JStlc.Eval
import Language.JStlc.Compile
import Language.JStlc.JS

example :: Term as ('ListTy 'IntTy)
example = App (Lam "x" SIntTy (Cons (Var IZ) (Lit [3, 2, 1])))
              (Lit 4)

main :: IO ()
main = do
  print example
  putStr "=eval=> "
  print $ eval example

  let js = compile example
  putStr "=compile=> "
  print js
  putStr "=emit=> "
  TIO.putStrLn $ emit js
