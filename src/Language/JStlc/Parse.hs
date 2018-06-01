{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JStlc.Parse (
    Parser
  , ident
  , ty
  , term
  , stmt
  , prog
  , only
  , space
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

import Data.Nat
import Language.JStlc.Types
import Language.JStlc.Unchecked

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

comma :: Parser T.Text
comma = symbol ","

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

-- TODO: existential encoding to support nested list literals
-- TLDR: we need an existential literal, where typechecking can
--   validate that the contained elements are type-consistent

litList :: Parser (UTerm n)
litList =  enclosed "[" "]" $
      try (ULit <$> sepBy1 integer comma)
  <|> try (ULit <$> sepBy1 qString comma)
  <|> (ULit <$> sepBy1 bool comma)

lit :: Parser (UTerm n)
lit =  try (ULit <$> qString)
   <|> try (ULit <$> integer)
   <|> try (ULit <$> bool)
   <|> litList
   -- TODO literal lists

lambda :: Parser (UTerm n)
lambda = do
  lexeme "\\"
  (x, t) <- annotated ident
  lexeme "=>"
  body <- term
  return $ runExSTy (toExSTy t) $ \s -> ULam x s body

letRec :: Parser (UTerm n)
letRec = do
  "let"
  space1
  "rec"
  space1
  (x, typ) <- annotated ident
  lexeme "="
  t <- term
  "in"
  space1
  u <- term
  return $ runExSTy (toExSTy typ) (\s -> ULetRec x s t u)

nonLRTerm :: Parser (UTerm n)
nonLRTerm = 
      try (UVar <$> ident)
  <|> try lambda
  <|> try ((\(_, t) -> runExSTy (toExSTy t) $ \s -> UNone s)
        <$> annotated (lexeme "none"))
  <|> try (UFix <$> ("fix" *> space1 *> term))
  <|> try (USome <$> ("some" *> space1 *> term))
  <|> try ((\(_, t) -> runExSTy (toExSTy t) $ \s -> UNil s)
        <$> annotated (lexeme "nil"))
  <|> try (UIfThenElse <$> ("if" *> space1 *> term)
                       <*> ("then" *> space1 *> term)
                       <*> ("else" *> space1 *> term))
  <|> try (ULet <$> ("let" *> space1 *> ident)
                <*> (lexeme "=" *> term)
                <*> ("in" *> space1 *> term))
  <|> try letRec
  <|> try (UFoldL <$> ("foldl" *> space1 *> nonLRTerm)
                  <*> nonLRTerm
                  <*> term)
  <|> try (UMap <$> ("map" *> space1 *> nonLRTerm) <*> term)
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

defineRec :: Parser (UStmt n ('S n))
defineRec = do
  "rec"
  space1
  (x, typ) <- annotated ident
  lexeme "="
  t <- term
  lexeme ";"
  return $ runExSTy (toExSTy typ) (\s -> UDefineRec x s t)

defineTyped :: Parser (UStmt n ('S n))
defineTyped = do
  (x, typ) <- annotated ident
  lexeme "="
  t <- term
  lexeme ";"
  return $ runExSTy (toExSTy typ) (\s -> UDefineTyped x s t)

stmt :: Parser (UStmt n ('S n))
stmt =  try defineRec
    <|> try defineTyped
    <|> (UDefine <$> ident <*> (lexeme "=" *> term <* lexeme ";"))

prog :: Parser ExUProg
prog = go SZ UEmptyProg where
  go :: SNat n -> UProg n -> Parser ExUProg
  go n p = do
    s <- optional stmt
    case s of
      Just s' -> go (SS n) (p :&?: s')
      Nothing -> return $ ExUProg (\k -> k n p)

only :: Parser a -> Parser a
only = (<* eof)
