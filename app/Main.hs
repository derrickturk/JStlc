{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.FilePath
import qualified Data.Text.IO as TIO

import Language.JStlc.Types
import Language.JStlc.Parse
import Language.JStlc.Check
import Language.JStlc.Eval
import Language.JStlc.Compile
import Language.JStlc.JS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> banner >> repl
    _ -> mapM_ compileFile args

banner :: IO ()
banner =
  putStrLn "       __ ____  __   __" >>
  putStrLn "      / // __/_/ /_ / /" >>
  putStrLn "  __ / //_  //_  _// /___  v0.1" >>
  putStrLn " / // /__/ /  / / / // _/  by dwt @ terminus data science, LLC" >>
  putStrLn "/___ //___/  /_/ /_//__/   (c) 2018" >>
  putStrLn ""

-- TODO: catch IO errors
compileFile :: FilePath -> IO ()
compileFile path = do
  let outPath = dropExtension path <.> "js"
  src <- TIO.readFile path
  -- TODO: stuff (need prog/stmt parsers)
  TIO.writeFile outPath src

repl :: IO ()
repl = do
  line <- TIO.getLine
  if line == "quit"
    then return ()
    else do
      let parsed = parse (only term) "(REPL)" line
      case parsed of
        Left e -> putStrLn (parseErrorPretty e) >> repl
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
