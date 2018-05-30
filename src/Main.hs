{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Language.JStlc.Types
import Language.JStlc.Parse
import Language.JStlc.Syntax
import Language.JStlc.Check
import Language.JStlc.Eval
import Language.JStlc.Compile
import Language.JStlc.JS

example :: Term as ('ListTy 'IntTy)
example = App (Lam "x" SIntTy (Cons (Var IZ) (Lit [3, 2, 1])))
              (Lit 4)

example2 :: Term as ('OptionTy 'BoolTy)
example2 = MapOption (Lam "x" SBoolTy (BinOpApp Or (Var IZ) (Lit True)))
                     (Some (BinOpApp Eq (Lit (17 :: Integer)) (Lit 21)))

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

  repl

repl :: IO ()
repl = do
  line <- TIO.getLine
  if line == "quit"
    then return ()
    else do
      let parsed = parse term "(REPL)" line
      case parsed of
        Left e -> print e >> repl
        Right ut -> do
          putStr "=parse=> "
          print ut
          case check ut of
            Left e -> print e >> repl
            Right exT -> runExTerm exT $ \s t -> do
              putStr "=check=> "
              putStrLn $ show t ++ " : " ++ show (unSTy s)
              putStr "=eval=> "
              putStrLn $ showVal s (eval t)
              let js = compile t
              putStr "=compile=> "
              print js
              putStr "=emit=> "
              TIO.putStrLn $ emit js
              repl
