{-# LANGUAGE DataKinds #-}

module Main where

import Language.JStlc.Syntax
import Language.JStlc.Eval
import Language.JStlc.Compile

example :: Term as ('ListTy 'IntTy)
example = App (Lam "x" SIntTy (Cons (Var IZ) (Lit [3, 2, 1])))
              (Lit 4)

main :: IO ()
main = do
  print example
  putStr "=> "
  print $ eval example
  putStr "=JS=> "
  print $ compile example
