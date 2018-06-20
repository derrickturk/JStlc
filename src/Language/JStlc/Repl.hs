{-# LANGUAGE GADTs, TypeOperators, DataKinds, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Repl (
    ReplState(..)
  , ExReplState
  , runExReplState
  , Repl
  , ReplError(..)
  , initReplState
  , replPrompt
  , replStep
  , runRepl
  , runReplIO_
  , replMain
  , replParseCommand
) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError, catchIOError, ioeGetErrorString)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.State.Class

import Control.Monad.State.Strict
import Control.Monad.Except

import Data.Nat
import Data.Vect

import Language.JStlc.Types
import Language.JStlc.Unchecked
import Language.JStlc.Syntax as S
import Language.JStlc.Parse
import Language.JStlc.Check as Ck
import Language.JStlc.Eval as E
import Language.JStlc.Compile as C
import Language.JStlc.JS
import Language.JStlc.Pretty.Class

data ReplState (n :: Nat) (as :: TyCtxt n) =
  ReplState { program :: Prog as
            , names :: NameCtxt as
            , vals :: Ctxt as
            }

newtype ExReplState =
  ExReplState { runExReplState :: forall r .
                                  (forall n as . SNat n
                                              -> STyCtxt as
                                              -> ReplState n as
                                              -> r)
                               -> r
              }

newtype Repl a =
  Repl { runRepl :: forall m . (MonadState ExReplState m,
                                MonadError ReplError m,
                                MonadIO m)
                             => m a
       }

data ReplCommand =
    EvalTerm T.Text
  | TypeTerm T.Text
  | ParseTerm T.Text
  | CompileTerm T.Text
  | InspectTerm T.Text
  | ExecStmt T.Text
  | ParseStmt T.Text
  | CompileStmt T.Text
  | InspectStmt T.Text
  | WriteCompiledProg T.Text
  | ShowCtxts
  | Help
  | Quit
  deriving Show

data ReplError =
    ReplParseError ParseError
  | ReplTypeError TypeError
  | InvalidReplCommand T.Text
  | ReplIOError IOError
  deriving Show

initReplState :: ExReplState
initReplState = ExReplState $ \k ->
  k SZ STyNil (ReplState EmptyProg VNil HVNil)

replPrompt :: Repl ()
replPrompt = Repl $ liftIO $ do
  putStrLn ""
  putStr "JStlc> "
  liftIO (hFlush stdout)

replStep :: ReplCommand -> Repl ()
replStep (EvalTerm src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> do
    ut <- runRepl $ replParseTerm src
    exT <- liftEither $ mapLeft ReplTypeError $
      check' (names rs) ts ut
    runExTerm exT $ \s t ->
      liftIO $ putStrLn $ showVal s (eval' (vals rs) t)

replStep (TypeTerm src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> do
    ut <- runRepl $ replParseTerm src
    exT <- liftEither $ mapLeft ReplTypeError $
      check' (names rs) ts ut
    runExTerm exT $ \s t -> liftIO $ do
      putStr (show t)
      putStr " : "
      print (unSTy s)

replStep (ParseTerm src) = Repl $ do
  ut <- runRepl $ replParseTerm src
  liftIO $ print ut

replStep (CompileTerm src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> do
    ut <- runRepl $ replParseTerm src
    exT <- liftEither $ mapLeft ReplTypeError $
      check' (names rs) ts ut
    let (_, cNames) = compileProg' (program rs)
    runExTerm exT $ \_ t ->
      liftIO $ TIO.putStrLn $ emit $ compile' cNames t

replStep (InspectTerm src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> do
    ut <- runRepl $ replParseTerm src
    liftIO $ putStr "=parse=> "
    liftIO $ print ut
    exT <- liftEither $ mapLeft ReplTypeError $
      check' (names rs) ts ut
    let (_, cNames) = compileProg' (program rs)
    runExTerm exT $ \s t -> liftIO $ do
      putStr "=check=> "
      putStrLn $ show t ++ " : " ++ show (unSTy s)
      putStr "=eval=> "
      putStrLn $ showVal s (eval' (vals rs) t)
      let js = compile' cNames t
      putStr "=compile=> "
      print js
      putStr "=emit=> "
      TIO.putStrLn $ emit js

replStep (ExecStmt src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \n ts rs -> do
    us <- runRepl $ replParseStmt src
    exNS <- liftEither $ mapLeft ReplTypeError $
      checkStmt' (names rs) ts us
    runExNamedStmt exNS $ \ts' names' st -> do
      let vals' = evalStmt (vals rs) st
          program' = program rs :&: st
      put $ ExReplState $ \k ->
        k (SS n) ts' (ReplState program' names' vals')

replStep (ParseStmt src) = Repl $ do
  us <- runRepl $ replParseStmt src
  liftIO $ print us

replStep (CompileStmt src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> do
    us <- runRepl $ replParseStmt src
    exSt <- liftEither $ mapLeft ReplTypeError $
      checkStmt (names rs) ts us
    let (_, cNames) = compileProg' (program rs)
    runExStmt exSt $ \_ st ->
      liftIO $ TIO.putStr $ emit $ compileStmt cNames st

replStep (InspectStmt src) = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> do
    us <- runRepl $ replParseStmt src
    liftIO $ putStr "=parse=> "
    liftIO $ print us
    exSt <- liftEither $ mapLeft ReplTypeError $
      checkStmt (names rs) ts us
    let (_, cNames) = compileProg' (program rs)
    runExStmt exSt $ \(t ::: _) st -> liftIO $ do
      putStr "=check=> "
      print st
      putStr "=eval=> "
      let (v ::> _) = evalStmt (vals rs) st
      putStrLn $ showVal t v
      let js = compileStmt cNames st
      putStr "=compile=> "
      print js
      putStr "=emit=> "
      TIO.putStr $ emit js

replStep (WriteCompiledProg outfile) = Repl $ do
  let path = T.unpack outfile
  exRS <- get
  runExReplState exRS $ \_ _ rs -> runRepl $ replWrapIO $
    TIO.writeFile path $ emit $ compileProg $ program rs

replStep ShowCtxts = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> liftIO $ do
    putStr "names: "
    print $ names rs
    putStr "vals: "
    putStrLn $ showCtxt ts (vals rs)

replStep Help = Repl $ liftIO $ TIO.putStr helpMsg
replStep Quit = Repl $ liftIO exitSuccess

replParseTerm :: T.Text -> Repl (UTerm n)
replParseTerm src = Repl $ liftEither $ mapLeft ReplParseError $
  parse (space *> only term) "(REPL)" src

replParseStmt :: T.Text -> Repl (UStmt n ('S n))
replParseStmt src = Repl $ liftEither $ mapLeft ReplParseError $
  parse (space *> only stmt) "(REPL)" src

replWrapIO :: IO () -> Repl ()
replWrapIO m = Repl $ do
  m' <- liftIO $ fallibly m
  liftEither $ mapLeft ReplIOError $ m'
  where
    fallibly :: IO () -> IO (Either IOError ())
    fallibly io = (Right <$> io) `catchIOError` (\e -> return (Left e))

runReplIO_ :: Repl a -> ExReplState -> IO ()
runReplIO_ (Repl r) s = () <$ execStateT (runExceptT r) s

replMain :: Repl ()
replMain = Repl $ do
  runRepl replPrompt
  do
    cmd <- runRepl replParseCommand
    runRepl $ replStep cmd
    `catchError` (\e -> liftIO $ TIO.putStrLn $ pretty e)
  runRepl replMain

replParseCommand :: Repl ReplCommand
replParseCommand = Repl $ do
  line <- liftIO $ TIO.getLine `catchIOError` handler
  case parseCommand line commandDict of
    Just cmd -> return cmd
    Nothing -> throwError $ InvalidReplCommand line
  where
    handler e = if isEOFError e
      then exitSuccess
      else ioError e
    parseCommand l [] = if T.head l == ':'
      then Nothing
      else Just $ EvalTerm l
    parseCommand l ((c, f):dict) = case T.stripPrefix c l of
      Just rest -> Just $ f $ T.stripStart rest
      Nothing -> parseCommand l dict

commandDict :: [(T.Text, T.Text -> ReplCommand)]
commandDict = [ (":quit", const Quit)
              , (":help", const Help)
              -- these need to come first as some later commands
              --   are prefixes of these
              , (":exec", ExecStmt)
              , (":parseStmt", ParseStmt)
              , (":compileStmt", CompileStmt)
              , (":inspectStmt", InspectStmt)
              , (":type", TypeTerm)
              , (":parse", ParseTerm)
              , (":compile", CompileTerm)
              , (":inspect", InspectTerm)
              , (":write", WriteCompiledProg)
              ]

showCtxt :: STyCtxt as -> Ctxt as -> String
showCtxt STyNil HVNil = "VNil"
showCtxt (t ::: ts) (x ::> xs) = showVal t x ++ " ::> " ++ showCtxt ts xs

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right e) = Right e

helpMsg :: T.Text
helpMsg =
  ":help - show this message\n" <>
  ":quit - exit JStlc REPL\n" <>
  ":type expr - parse and typecheck an expression\n" <>
  ":parse expr - parse an expression and show the untyped AST\n" <>
  ":compile expr - compile an expression to a JS expression\n" <>
  ":inspect expr - parse, check, and compile an expression verbosely\n" <>
  ":exec stmt - execute a statement, such as\n" <>
  "    x = 17;\n" <>
  "    f: Int -> Int = \\x : Int => 23;\n" <>
  "    rec f: Int -> Int = \\x: Int => if x == 0 then 1 else x * f (x - 1);\n" <>
  ":parseStmt stmt - parse a statement and show the untyped AST\n" <>
  ":compileStmt stmt - compile a statement to a JS definition\n" <>
  ":inspectStmt stmt - parse, check, and compile a statement verbosely\n" <>
  ":write filename - compile all current definitions to specified JS file\n"

instance Show ExReplState where
  show exRS = runExReplState exRS $ \_ ts rs ->
    "ReplState { program = " ++ show (program rs) ++
    ", names = " ++ show (names rs) ++
    ", vals = " ++ showCtxt ts (vals rs) ++ " }"

instance Pretty ReplError where
  pretty (ReplParseError e) = T.stripEnd $ T.pack $ parseErrorPretty e
  pretty (ReplTypeError e) = pretty e
  pretty (InvalidReplCommand c) = "Invalid REPL command: " <> c
  pretty (ReplIOError e) = T.pack $ ioeGetErrorString e
