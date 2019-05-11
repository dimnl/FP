module Set5 where

import Data.List
import Data.Functor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Test.QuickCheck

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs

------------------------------------------------------------------------------
-- Ex 2
------------------------------------------------------------------------------
languageDef =
  emptyDef { Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedOpNames  = [ "+", "*", "==", "="]
           , Token.reservedNames    = [ "if"
                                      , "then"
                                      , "else"
                                      , "dec"
                                      , "function"
                                      ]
           }
-- lexer
lexer = Token.makeTokenParser languageDef
-- functions for tokens
identifier = Token.identifier lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer

------------------------------------------------------------------------------
-- Ex 3
------------------------------------------------------------------------------
-- define Expr; commented out for later exercise
-- data Expr = Add   Expr Expr
--           | Mult  Expr Expr
--           | Const Integer
--           | Var   String
--           deriving (Show, Eq)

-- 1 ; commented out for later exercise
-- parseFactor :: Parser Expr
-- parseFactor = parsePar <|> parseNum <|> parseIdent

parsePar :: Parser Expr -- added for 3.
parsePar = parens parseExpr'

parseNum :: Parser Expr
parseNum = do
  num <- many1 digit
  return (Const $ read num)

parseIdent :: Parser Expr
parseIdent = do
  ident <- many1 alphaNum
  return (Var $ ident)

-- 2
parseTerm :: Parser Expr
parseTerm = do
  f1 <- parseFactor
  loop f1
  where checkOp f1 = do
            spaces
            op <- symbol "*"
            spaces
            f2 <- parseFactor
            loop $ Mult f1 f2
        loop f = try(checkOp f) <|> return f

-- Version that only works with zero or one factors:
-- parseTerm = try (Mult <$> (parseFactor <* spaces <* symbol "*")
--                       <*> (spaces *> parseFactor))
--           <|> parseFactor
-- 3
parseExpr :: Parser Expr
parseExpr = do
  t1 <- parseTerm
  loop t1
  where checkOp t1 = do
            spaces
            op <- symbol "+"
            spaces
            t2 <- parseTerm
            loop $ Add t1 t2
        loop t = try(checkOp t) <|> return t

-- Version that only works with zero or one terms:
-- parseExpr = try (Add <$> (parseTerm <* spaces <* symbol "+")
--                      <*> (spaces *> parseTerm))
--           <|> parseTerm

test3_3 = parser parseExpr "3*(1+a)"

------------------------------------------------------------------------------
-- Ex 4
------------------------------------------------------------------------------
-- 1
data Cond = Cond Expr Expr
          deriving (Show, Eq)

-- data Expr = Add   Expr Expr
--           | Mult  Expr Expr
--           | Const Integer
--           | Var   String
--           | If    Cond Expr Expr
--           | Dec   Expr
--           deriving (Show, Eq)
-- 2
parseCondition :: Parser Cond
parseCondition = Cond <$> (parseExpr' <* spaces <* symbol "==")
                      <*> (spaces *> parseExpr')

parseIf :: Parser Expr
parseIf = If <$> (reserved "if"   *> parseCondition <* spaces)
             <*> (reserved "then" *> parseExpr'      <* spaces)
             <*> (reserved "else" *> parseExpr'      <* spaces)

parseDec :: Parser Expr
parseDec = Dec <$> (reserved "dec" *> parseExpr')

-- 3
parseExpr' :: Parser Expr
parseExpr' = parseDec <|> parseIf <|> parseExpr

------------------------------------------------------------------------------
-- Ex 5
------------------------------------------------------------------------------
data FunDef = Function Expr Expr Expr
            deriving (Show, Eq)

data Expr = Add   Expr Expr
          | Mult  Expr Expr
          | Const Integer
          | Var   String
          | If    Cond Expr Expr
          | Dec   Expr
          | Eval  Expr Expr
          deriving (Show, Eq)
-- 2
parseFunction :: Parser FunDef
parseFunction = Function <$> (reserved "function" *> parseIdent)
                         <*> (spaces *> parseIdent)
                         <*> (spaces *> symbol "=" *> parseExpr')
parseFactor :: Parser Expr
parseFactor = parsePar <|> parseNum <|> try(parseIdentPar) <|> parseIdent

parseIdentPar :: Parser Expr
parseIdentPar = Eval <$> parseIdent <*> parsePar
-- 3
parserFun :: String -> FunDef
parserFun = parser parseFunction

fib :: FunDef
fib = parserFun "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"

------------------------------------------------------------------------------
-- Ex 6
------------------------------------------------------------------------------
evalfun :: FunDef -> Integer -> Integer
evalfun (Function fname argname expr) i = evalExpr expr i expr

evalExpr :: Expr -> Integer -> Expr -> Integer
evalExpr f i (Add  x y) = evalExpr f i x + evalExpr f i y
evalExpr f i (Mult x y) = evalExpr f i x * evalExpr f i y
evalExpr f i (Const x)  = x
evalExpr f i (Var x)    = i
evalExpr f i (If c f1 f2)
 | evalCond f i c       = evalExpr f i f1
 | otherwise            = evalExpr f i f2
evalExpr f i (Dec x)    = evalExpr f i x - 1
evalExpr f i (Eval x y) = evalExpr f (evalExpr f i y) f

evalCond :: Expr -> Integer -> Cond -> Bool
evalCond f i (Cond x y) = evalExpr f i x == evalExpr f i y

-- 2
-- Checks if in the evaluation (xˆ2 + 2*x + 1) == (x+1)ˆ2
fn  = (evalfun . parserFun) "function f x = x*x + 2*x + 1"
fn' = (evalfun . parserFun) "function f x = (x+1) * (x+1)"
prop_fn n = n >= 0 ==>fn n == fn' n

-- 3
factorial :: Integer -> Integer
factorial = (evalfun . parserFun) "function factorial x = if x == 0 then 1 else factorial(dec x) * x"
prop_factorial n = n >= 0 ==> factorial n == product [1..n]
