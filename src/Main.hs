{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Language.JStlc.Types
import Language.JStlc.Syntax
import Language.JStlc.Eval
import Language.JStlc.Compile
import Language.JStlc.JS

example :: Term as ('ListTy 'IntTy)
example = App (Lam "x" SIntTy (Cons (Var IZ) (Lit [3, 2, 1])))
              (Lit 4)

example2 :: Term as ('OptionTy 'BoolTy)
example2 = MapOption (Lam "x" SBoolTy (BinOpApp Or (Var IZ) (Lit True)))
                     (Some (BinOpApp Eq (Lit (17 :: Int)) (Lit 21)))

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

  print example2
  putStr "=eval=> "
  print $ eval example2

  let js2 = compile example2
  putStr "=compile=> "
  print js2
  putStr "=emit=> "
  TIO.putStrLn $ emit js2
