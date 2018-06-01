{-# LANGUAGE GADTs, TypeOperators, DataKinds, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Repl (
    ReplState(..)
  , ExReplState
  , runExReplState
  , Repl
  , ReplError(..)
  , initReplState
  , replStep
  , runRepl
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError, catchIOError, ioError)
import System.Exit (exitSuccess)

import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.State.Class

import Data.Nat
import Language.JStlc.Types
import Language.JStlc.Unchecked
import Language.JStlc.Syntax as S
import Language.JStlc.Parse
import Language.JStlc.Check as Ck
import Language.JStlc.Eval as E
import Language.JStlc.Compile as C
import Language.JStlc.JS

data ReplState n as = ReplState { program :: Prog as
                                , names :: UNameCtxt n
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
  | InspectStmt T.Text
  | CompileStmt T.Text
  | CompileProg T.Text
  | ShowCtxts
  | Help
  | Quit
  deriving Show

data ReplError =
    ReplParseError ParseError
  | ReplTypeError TypeError
  | InvalidReplCommand T.Text
  deriving Show

initReplState :: ExReplState
initReplState = ExReplState $ \k ->
  k SZ STyNil (ReplState EmptyProg UNameNil CNil)

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

replStep ShowCtxts = Repl $ do
  exRS <- get
  runExReplState exRS $ \_ ts rs -> liftIO $ do
    putStr "names: "
    print $ names rs
    putStr "vals: "
    putStrLn $ showCtxt ts (vals rs)

replStep Help = Repl $ liftIO $ TIO.putStrLn helpMsg
replStep Quit = Repl $ liftIO exitSuccess

replParseTerm :: T.Text -> Repl (UTerm n)
replParseTerm src = Repl $ liftEither $ mapLeft ReplParseError $
  parse (space *> only term) "(REPL)" src

showCtxt :: STyCtxt as -> Ctxt as -> String
showCtxt STyNil CNil = "CNil"
showCtxt (t ::: ts) (x E.:> xs) = showVal t x ++ " :> " ++ showCtxt ts xs

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right e) = Right e

helpMsg :: T.Text
helpMsg = "help coming soon"

instance Show ExReplState where
  show exRS = runExReplState exRS $ \_ ts rs ->
    "ReplState { program = " ++ show (program rs) ++
    ", names = " ++ show (names rs) ++
    ", vals = " ++ showCtxt ts (vals rs) ++ " }"
