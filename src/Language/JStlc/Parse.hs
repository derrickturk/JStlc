{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Parse where

import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

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
           , "fold"
           ]
