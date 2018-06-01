{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.FilePath
import System.IO
import qualified Data.Text.IO as TIO

import Language.JStlc.Types
import Language.JStlc.Parse
import Language.JStlc.Unchecked
import Language.JStlc.Check
import Language.JStlc.Eval
import Language.JStlc.Compile
import Language.JStlc.JS
import Language.JStlc.Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> banner >> runReplIO_ replMain initReplState
    _ -> mapM_ compileFile args

banner :: IO ()
banner =
  putStrLn "       __ ____  __   __" >>
  putStrLn "      / // __/_/ /_ / /" >>
  putStrLn "  __ / //_  //_  _// /___  v0.1" >>
  putStrLn " / // /__/ /  / / / // _/  by dwt @ terminus data science, LLC" >>
  putStrLn "/___ //___/  /_/ /_//__/   (c) 2018"

-- TODO: catch IO errors
compileFile :: FilePath -> IO ()
compileFile path = do
  let outPath = dropExtension path <.> "js"
  src <- TIO.readFile path
  -- TODO: stuff (need prog/stmt parsers)
  let parsed = parse (space *> only prog) path src
  case parsed of
    Left e -> hPutStrLn stderr (parseErrorPretty e)
    Right exUProg -> runExUProg exUProg $
      \_ up -> case checkProg up of
        Left e -> do
          hPutStr stderr $ path ++ ": "
          hPrint stderr e
        Right exP -> runExProg exP $
          \_ p -> TIO.writeFile outPath $ emit $ compileProg p

repl :: IO ()
repl = do
  putStrLn ""
  line <- TIO.getLine
  if line == "quit"
    then return ()
    else do
      let parsed = parse (space *> only term) "(REPL)" line
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
