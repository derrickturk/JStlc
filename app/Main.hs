{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.FilePath
import System.IO
import qualified Data.Text.IO as TIO

import Language.JStlc.Parse
import Language.JStlc.Unchecked
import Language.JStlc.Check
import Language.JStlc.Compile
import Language.JStlc.JS
import Language.JStlc.Repl
import Language.JStlc.Pretty.Class

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
          TIO.hPutStrLn stderr (pretty e)
        Right exP -> runExProg exP $
          \_ p -> TIO.writeFile outPath $ emit $ compileProg p
