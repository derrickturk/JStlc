{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Parse (
    Parser
  , ident
  , ty
  , term
  , only
  , parse
  , parseTest
  , parseTest'
  , runParser
  , runParser'
  , parseErrorPretty
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
integer = lexeme $
      try $ enclosed "(" ")" (lexeme $ L.signed space L.decimal)
  <|> L.decimal

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
           , "rec"
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

ty :: Parser Ty
ty =  try (binOpRec FnTy baseTy (lexeme "->" *> ty))
  <|> baseTy

baseTy :: Parser Ty
baseTy = lexeme $
      IntTy <$ "Int"
  <|> BoolTy <$ "Bool"
  <|> StringTy <$ "String"
  <|> ListTy <$> enclosed "[" "]" ty
  <|> OptionTy <$> ("?" *> baseTy)
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

lambda :: Parser (UTerm n)
lambda = do
  lexeme "\\"
  (x, t) <- annotated ident
  lexeme "=>"
  body <- term
  return $ runExSTy (toExSTy t) $ \s -> ULam x s body

nonLRTerm :: Parser (UTerm n)
nonLRTerm = 
      try (UVar <$> ident)
  <|> try lambda
  <|> try ((\(_, t) -> runExSTy (toExSTy t) $ \s -> UNone s)
        <$> annotated "none")
  <|> try (UFix <$> (lexeme "fix" *> space *> term))
  <|> try (USome <$> (lexeme "some" *> space *> term))
  <|> try ((\(_, t) -> runExSTy (toExSTy t) $ \s -> UNil s)
        <$> annotated "nil")
  <|> try (UIfThenElse <$> (lexeme "if" *> space *> term)
                       <*> (lexeme "then" *> space *> term)
                       <*> (lexeme "else" *> space *> term))
  <|> try (ULet <$> (lexeme "let" *> space *> ident)
                <*> (lexeme "=" *> term)
                <*> (lexeme "in" *> space *> term))
  <|> try (UFoldL <$> (lexeme "foldl" *> space *> nonLRTerm)
                  <*> nonLRTerm
                  <*> term)
  <|> try (UMap <$> (lexeme "map" *> space *> nonLRTerm) <*> term)
  <|> try lit -- must precede ()-enclosed term
  <|> lexeme (enclosed "(" ")" term)

binOp :: UBinOp -> T.Text -> Parser UBinOp
binOp op l = op <$ lexeme (string l)

-- for operators which are a prefix of other operators
binOpAmbPrefix :: UBinOp -> T.Text -> Parser UBinOp
binOpAmbPrefix op l =
  op <$ (lexeme $ try $ string l <* notFollowedBy punctuationChar)

opTable :: [[Operator Parser (UTerm n)]]
opTable = [ [ InfixL (UBinOpApp <$> binOp UMul "*")
            , InfixL (UBinOpApp <$> binOp UDiv "/")
            ]
          , [ InfixL (UBinOpApp <$> binOpAmbPrefix UAdd "+")
            , InfixL (UBinOpApp <$> binOp USub "-")
            ]
          , [ InfixR (UCons <$ lexeme "::")
            , InfixL (UBinOpApp <$> binOp UAppend "++")
            , InfixL (UBinOpApp <$> binOpAmbPrefix UStrCat "&")
            ]
          , [ InfixN (UBinOpApp <$> binOp UEq "==") ]
          , [ InfixL (UBinOpApp <$> binOp UAnd "&&") ]
          , [ InfixL (UBinOpApp <$> binOp UOr "||") ]
          , [ InfixR (UApp <$ lexeme "$") ]
          ]

appTerm :: Parser (UTerm n)
appTerm =  try (binOpRec UApp nonLRTerm nonLRTerm)
       <|> nonLRTerm

term :: Parser (UTerm n)
term = makeExprParser appTerm opTable

only :: Parser a -> Parser a
only = (<* lexeme eof)
