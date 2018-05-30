{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Parse (
    Parser
  , ident
  , ty
  , term
  , parseTest
  , parseTest'
  , runParser
  , runParser'
) where

import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Language.JStlc.Types
import Language.JStlc.Unchecked

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

enclosed :: T.Text -> T.Text -> Parser a -> Parser a
enclosed left right = between (symbol left) (symbol right)

integer :: Parser Integer
integer = lexeme $ L.signed space L.decimal

bool :: Parser Bool
bool = lexeme $ True <$ "true" <|> False <$ "false"

qString :: Parser T.Text
qString = lexeme $ char '"' *> (T.pack <$> manyTill L.charLiteral (char '"'))

keywords :: [T.Text]
keywords = [ "if"
           , "then"
           , "else"
           , "true"
           , "false"
           , "let"
           , "in"
           , "some"
           , "none"
           , "nil"
           , "fix"
           , "map"
           , "foldl"
           ]

ident :: Parser T.Text
ident = lexeme $ do
  var <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if var `elem` keywords
    then failure
      (Just $ Label $ NE.fromList "keyword")
      (S.singleton $ Label $ NE.fromList "identifier")
    else return var

binOpRec :: (a -> b -> a) -> Parser a -> Parser b -> Parser a
binOpRec f base rest = foldl f <$> base <*> some rest

binOpsRec :: Parser (a -> b -> a) -> Parser a -> Parser b -> Parser a
binOpsRec op base rest =
  foldl (\b (o, r) -> o b r) <$> base <*> some ((,) <$> op <*> rest)

ty :: Parser Ty
ty =  try (binOpRec FnTy baseTy (lexeme "->" *> ty))
  <|> baseTy

baseTy :: Parser Ty
baseTy = lexeme $
      IntTy <$ "Int"
  <|> BoolTy <$ "Bool"
  <|> StringTy <$ "String"
  <|> ListTy <$> enclosed "[" "]" ty
  <|> OptionTy <$> (lexeme "?" *> baseTy)
  <|> enclosed "(" ")" ty

annotated :: Parser a -> Parser (a, Ty)
annotated p = do
  x <- p
  lexeme ":"
  t <- ty
  return (x, t)

lit :: Parser (UTerm n)
lit =  try (ULit <$> qString)
   <|> try (ULit <$> integer)
   <|> (ULit <$> bool)
   -- TODO literal lists

term :: Parser (UTerm n)
term =  try (UVar <$> ident)
    <|> try lit
    <|> try ((\(_, t) -> runExSTy (toExSTy t) $ \s -> UNil s)
          <$> annotated "nil")
