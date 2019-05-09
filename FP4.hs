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

la :: [Char] -> [Char] -> Bool -- look ahead function
la xs el = length xs > 0 && (head xs) `elem` el

parseFactor2 :: String -> (BinTree Char Value, String)
parseFactor2 (x:xs)
  | isDigit x  = num                  -- num
  | isAlpha x  = identifier           -- identifier
  | x == '('  = (tE, tail rE)         -- '(' E ')'
  | otherwise = error ("error parsing factor: " ++ show [x])
  where num          = (Leaf $ Const $ read [x], xs)
        identifier   = (Leaf $ Id  [x] , xs)
        (tE, rE)     = parseExpr2 xs

parseTerm2 :: String -> (BinTree Char Value, String)
parseTerm2 s
  | la rF "*" = (Node '*' tF tT, rT) -- F '*' T
  | otherwise = (tF, rF)             -- F
  where (tF, rF) = parseFactor2 s
        (tT, rT) = parseTerm2 $ tail rF

parseExpr2 :: String -> (BinTree Char Value, String)
parseExpr2 s
  | la rT "+" = (Node '+' tT tE, rE) -- T '+' E
  | otherwise = (tT, rT)             -- T
  where (tT, rT) = parseTerm2 s
        (tE, rE) = parseExpr2 $ tail rT

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
laT :: [Token] -> [Token] -> Bool -- look ahead function
laT xs el = length xs > 0 && (head xs) `elem` el

parseFactor' :: [Token] -> (BinTree Token Token, [Token])
parseFactor' (num@(Num _): ts)      = (Leaf num, ts)
parseFactor' (ident@(Ident _): ts)  = (Leaf ident, ts)
parseFactor' (OpenPar: ts)          = (tE, tail rE)
  where (tE, rE)     = parseExpr' ts

parseTerm' :: [Token] -> (BinTree Token Token, [Token])
parseTerm' tl
  | laT rF [Mult] = (Node Mult tF tT, rT) -- F '*' T
  | otherwise = (tF, rF)             -- F
  where (tF, rF) = parseFactor' tl
        (tT, rT) = parseTerm' $ tail rF

parseExpr' :: [Token] -> (BinTree Token Token, [Token])
parseExpr' tl
  | laT rT [Add]  = (Node Add tT tE, rE)
  | otherwise   = (tT, rT)
  where (tT, rT) = parseTerm' tl
        (tE, rE) = parseExpr' $ tail rT

test3 :: IO ()
test3 = showBinTree $ fst $ parseExpr' $ tokenize "(2+4)*3"

-- Ex 15
eval :: String -> [(String, Int)] -> Int
eval s lu = evalTree  (fst $ parseExpr' $ tokenize s)  lu

evalTree :: BinTree Token Token -> [(String, Int)] -> Int
evalTree (Node token l r) lu =  (toOp token) (evalTree l lu) (evalTree r lu)
evalTree (Leaf (Num n)) lu   = n
evalTree (Leaf (Ident n)) lu = lookupTable n lu

toOp :: Token -> (Int -> Int -> Int)
toOp token
  | token == Mult   = (*)
  | token == Add    = (+)

lookupTable :: String -> [(String, Int)] -> Int
lookupTable var [] = 0
lookupTable var (t1:ts)
  | fst t1 == var = snd t1
  | otherwise       = lookupTable var ts

testEval :: Int
testEval = eval "(a123+ 4)*b2" [("a123", 2), ("b2", 3)]
