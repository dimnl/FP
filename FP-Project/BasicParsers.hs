-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Dim Hoogeveen (s1728938)
-- Student 2: Other Name (syyyyyyy)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
-- import Test.QuickCheck
import PComb

------------------------------------------------------------------
--  D-4: FP2.1
------------------------------------------------------------------
-- Finding lowercase letters only, as specified in D-1
letter :: Parser Char
letter = P p
  where p (Stream []) = []
        p (Stream (x:xs))
          | isLower x = [(x, Stream xs)]
          | otherwise = []

dig :: Parser Char
dig = P p
  where p (Stream []) = []
        p (Stream (x:xs))
          | isDigit x = [(x, Stream xs)]
          | otherwise = []

-- Simple tests
parseLetterFail    = runParser letter (Stream "1a12")
parseLetterSatisfy = runParser letter (Stream "a12")

parseDigFail    = runParser dig (Stream "a12")
parseDigSatisfy = runParser dig (Stream "1a12")

------------------------------------------------------------------
--  D-4: FP2.2
------------------------------------------------------------------
-- Run all parsers sequentially and return result second parser:
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

-- Parse, while skipping all whitespace (space, tab and newline)
whitespace :: Parser a -> Parser a
whitespace (P p) = P pR
  where pR (Stream xs) = p (Stream (filter (\c -> not $ isSpace c) xs))


-- Simple tests
parseBetweenFail = runParser (between (char '(') letter (char ')')) (Stream ")a)")
parseBetweenSatisfy = runParser (between (char '(') letter (char ')')) (Stream "(a)")
parseWhiteSpace = runParser (whitespace (char 'a'))  (Stream " \t  a  \n ")

------------------------------------------------------------------
--  D-4: FP2.3
------------------------------------------------------------------
-- Parses one or more occurences of p, separated by s
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = some (p <* s)

-- Parses zero or more occurences of p, separated by s
sep :: Parser a -> Parser b -> Parser [a]
sep p s = many (p <* s)

-- Tries to apply parser p, upon failure it results in x
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- Simple tests
parseSep1 = runParser (sep1 letter (char 'a')) (Stream "babarae")
parseSep = runParser (sep letter (char 'a')) (Stream "babarae")
parseOptionFail = runParser (option 'x' letter) (Stream "123")
parseOptionSatisfy = runParser (option 'x' dig) (Stream "123")

------------------------------------------------------------------
--  D-4: FP2.4
------------------------------------------------------------------
--  Parses a given String
string :: String -> Parser String
string "" = pure ""
string (x:xs) = (:) <$> char x <*> string xs

-- Parses a given identifier surrounded by whitespace
-- identifier starts with letter, followed by letters or digits
identifier :: Parser String
identifier = whitespace $ P p
  where p = (\x -> [([r1]  ++ r2, x2) | (r1, x1) <- f x, (r2, x2) <- g x1])
        f = runParser letter
        g = runParser (many (dig <|> letter))

-- Parses an integer surrounded by whitespace
integer :: Parser Int
integer = whitespace $ P p
  where p = \x -> [(read r, xs)| (r,xs) <- runParser digits x]
        digits = some dig

-- Parses a given String surrounde by whitespace
symbol :: String -> Parser ()
symbol s = whitespace $ P p
  where p = \x -> [((),xs)| (_,xs) <- runParser (string s) x]

-- Parses something between parentheses using the given parser
parens :: Parser a -> Parser a
parens p = between (char '(') p (char ')')

-- Parses something between braces using the given partner
braces :: Parser a -> Parser a
braces p = between (char '{') p (char '}')

-- Simple tests
parseStringSatisfy    = runParser (string "ab") (Stream "ab123b")
parseStringFail       = runParser (string "abc") (Stream "ab123b")
parseIdentifierSatisfy= runParser identifier (Stream "\t  ab123b")
parseIdentifierFail   = runParser identifier (Stream "1ab123b")
parseIntegerSatisfy   = runParser integer (Stream "\t  23b")
parseIntegerFail      = runParser integer (Stream "a1ab123b")
parseSymbolSatisfy    = runParser (symbol "=") (Stream "  =  123")
parseSymbolFail       = runParser (symbol "=") (Stream " 123 =  123")
parseParensSatisfy    = runParser (parens identifier) (Stream "(aa23a)")
parseParensFail       = runParser (parens identifier) (Stream "(123()")
parseBracesSatify     = runParser (braces identifier) (Stream "{aa23a}")
parseBracesFail       = runParser (braces identifier) (Stream "{123{}")
