{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Parse where

import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
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

string :: Parser T.Text
string = lexeme $ char '"' *> (T.pack <$> manyTill L.charLiteral (char '"'))

keywords :: [T.Text]
keywords = [ "if"
           , "then"
           , "else"
           , "true"
           , "false"
           , "let"
           , "in"
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
  <|> OptionTy <$> (lexeme "?" *> ty)
  <|> enclosed "(" ")" ty
