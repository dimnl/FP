module Set5 where

import FPCore -- added import

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

-- Uncomment for later exercises:
{-
-- Expression EDSL
data Expr = Const Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          deriving Show
-}
{-
fib :: Integer -> Integer
fib = (evalfun . parserFun)
 "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"
-}

-- Ex 2
languageDef =
  emptyDef { Token.identStart       = alphaNum
           , Token.identLetter      = alphaNum
           , Token.reservedOpNames  = [ "+", "*"]
           }
-- lexer
lexer = Token.makeTokenParser languageDef
-- functions for tokens
identifier = Token.identifier lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer

-- Ex 3
parseFactor :: Parser Expr
parseFactor = integer parseFactor
