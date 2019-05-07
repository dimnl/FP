-- Imports
import FPPrac.Trees
import Data.List
import Data.Char
import Test.QuickCheck
import Data.Maybe

-- Ex 11
data BinTree a b  = Leaf b
                  | Node a (BinTree a b) (BinTree a b)
                  deriving (Show, Eq)

pp :: (Show a, Show b) => BinTree a b -> RoseTree
pp (Leaf n)     = RoseNode (show n) []
pp (Node n l r) = RoseNode (show n) [pp l,  pp r]

showBinTree :: (Show a, Show b) => BinTree a b -> IO ()
showBinTree = showRoseTree . pp

-- ===================================================
-- No error handling in below exercises about parsing
-- ===================================================

-- Ex 12
-- BNF:
-- E -> T '+' E
-- E -> T
-- T -> F '*' T
-- T -> F
-- F -> [0..9]{1}

parseFactor :: String -> BinTree Char Int
parseFactor num = Leaf (read num :: Int)

parseTerm :: String -> BinTree Char Int
parseTerm  s
  | length s == 1 = parseFactor s
  | otherwise     = Node '*' (parseFactor (take 1 s)) (parseTerm (drop 2 s))

parseExpr :: String -> BinTree Char Int
parseExpr s
  | not $ isNothing i = Node '+' (parseTerm (take iInt s)) (parseExpr (drop (iInt + 1) s))
  | otherwise         = parseTerm s
  where i = findIndex (=='+') s
        iInt = fromJust i

test1 :: IO ()
test1 = showBinTree $ parseExpr "2+3*2+1"
-- 2
-- BNF:
-- E -> T '+' E
-- E -> T
-- T -> F '*' T
-- T -> F
-- F -> [0..9]{1}
-- F -> [a-zA-Z]{1}
-- F -> '(' E ')'

data Value = Const Int
           | Id String
           deriving Show

parseFactor2 :: String -> (BinTree Char Value, String)
parseFactor2 s@(x:xs)
  | isDigit x  = num
  | isAlpha x  = identifier
  | otherwise  = (t1,ys) -- '('
  where num         = (Leaf $ Const $ read [x], xs)
        identifier  = (Leaf $ Id  [x] , xs)
        (t1, ')':ys)    = parseExpr2 xs

parseTerm2 :: String -> (BinTree Char Value, String)
parseTerm2 s@(x:xs)
  | x == '('      = parseFactor2 s
  | length s == 1 = parseFactor2 s -- F
  | otherwise     = (Node '*' left right, rr) -- F '*' T
  where (left, rl) = parseFactor2 $ take 1 s
        (right, rr)= parseTerm2 $ drop 2 s

parseExpr2 :: String -> (BinTree Char Value, String)
parseExpr2 s@(x:xs)
  | isNothing i = parseTerm2 s -- T
  | x == '('    = parseTerm2 s
  | otherwise = (Node '+' l r, rr)
  where i = findIndex (=='+') s
        iInt = fromJust i
        (l, rl) = parseTerm2 (take iInt s)
        (r, rr) = parseExpr2 (drop (iInt +1) s)

test2 :: IO ()
test2 = showBinTree $ fst $ parseExpr2 "(2+4)*3"

-- Ex 13
data Token  = OpenPar     --  '('
            | ClosePar    --  ')'
            | Add         --  '+'
            | Mult        --  '*'
            | Num Int     --  one or more digits
            | Ident String   --  multiple letters/digits, start with letter
            deriving (Show, Eq)

digit = ['0'..'9']
letter = ['a'..'z'] ++ ['A'..'Z']
letdig = letter ++ digit

tokenize :: String -> [Token]
tokenize []               = []
tokenize (' ' : xs)       = tokenize xs --ignore spaces
tokenize ('\t': xs)       = tokenize xs --ignore tabs
tokenize ('(' : xs)       = OpenPar : tokenize xs
tokenize (')' : xs)       = ClosePar : tokenize xs
tokenize ('+' : xs)       = Add : tokenize xs
tokenize ('*' : xs)       = Mult : tokenize xs
tokenize s@(x:xs)
  | x `elem` letter       = Ident id : tokenize rem
  where (id, rem) = span (`elem` letdig) s
tokenize s@(x:xs)
  | x `elem` digit        = Num (read dig) : tokenize rem
  where (dig, rem) = span (`elem` digit) s

-- Ex 14
-- parseExpr' :: [Token] -> (BinTree Token Token, [Token])
-- parseExpr' s@(OpenPar : xs) =

-- Ex 15
-- eval :: String -> Int
-- eval = evalTree . parseExpr' . tokenize

evalTree :: BinTree Token Token -> [(String, Int)] -> Int
evalTree (Node token l r) lookupT =  (toOp token) (evalTree l lookupT) (evalTree r lookupT)
evalTree (Leaf (Num n)) lookupT   = n
evalTree (Leaf (Ident n)) lookupT = lookupTable n lookupT

toOp :: Token -> (Int -> Int -> Int)
toOp token
  | token == Mult   = (*)
  | token == Add    = (+)

lookupTable :: String -> [(String, Int)] -> Int
lookupTable var [] = 0
lookupTable var (t1:ts)
  | fst t1 == var = snd t1
  | otherwise       = lookupTable var ts
