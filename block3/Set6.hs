module Set6 where

import Data.Foldable
import Data.Monoid
import Text.Read
import Data.Char
import Control.Applicative
import Data.Maybe

import Test.QuickCheck
import Set5

------------------------------------------------------------------------------
--Exercise 3-FP7
------------------------------------------------------------------------------
instance Monoid Int where
  mempty = 0
  mappend = (+)

sumint :: [Int] -> Int
sumint = mconcat

------------------------------------------------------------------------------
-- Exercise 3-FP.8
------------------------------------------------------------------------------
data A a = A { fromA :: Maybe a }
         deriving Show

instance Monoid (A a) where
  mempty = A Nothing
  mappend (A (Just x)) _=  A (Just x)
  mappend (A Nothing) x = x
-- 2
firstInt :: [String] -> Maybe Int
firstInt s = fromA . mconcat . map A $ map readMaybe s

------------------------------------------------------------------------------
-- Exercise 3-FP.9
------------------------------------------------------------------------------
addstr :: String -> String -> Maybe String
addstr s1 s2 = case tryAdd of Nothing  -> Nothing
                              (Just s) -> Just (show s)
               where tryAdd = ((+) <$> readMaybe s1 <*> readMaybe s2 )

-- Below BinTree was already in file

-- data BinTree a = Leaf a
--                | Node (BinTree a) (BinTree a)
--
-- t :: BinTree Int
-- t = Node (Node
--             (Leaf 3)
--             (Node (Leaf 6)
--                   (Leaf 9)))
--          (Leaf 9)===-=
--
-- t2 :: BinTree Int
-- t2 = Node (Leaf 3) (Leaf 5)
--
-- inftree :: BinTree Int
-- inftree = Node (Leaf 1) inftree

------------------------------------------------------------------------------
-- Exercise 3-FP.10
------------------------------------------------------------------------------
data MyList a = Nil | Cons a (MyList a)
              deriving (Show, Eq)

mylst  = Cons 1   $ Cons 2   $ Cons 3   $ Nil
mylst2 = Cons 10  $ Cons 20  $ Cons 30  $ Nil
mylst3 = Cons 100 $ Cons 200 $ Cons 300 $ Nil

-- MyList functor instance from FP3.hs
instance Functor MyList where
  fmap f Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

-- 1
myzipWith3' :: (a -> b -> c -> d) -> MyList a -> MyList b -> MyList c -> MyList d
myzipWith3' _ Nil _ _ = Nil
myzipWith3' _ _ Nil _ = Nil
myzipWith3' _ _ _ Nil = Nil
myzipWith3' f (Cons x (xs)) (Cons y (ys)) (Cons z (zs)) = Cons (f x y z) (myzipWith3' f xs ys zs)

-- fromList as helper function for prop_myzipwith3'
fromList :: [a] -> MyList a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

prop_myzipwith3' :: Fun (Integer, Integer, Integer) Integer -> Bool
prop_myzipwith3' f = myzipWith3' (applyFun3 f) mylst mylst2 mylst3 ==
                     fromList (zipWith3 (applyFun3 f) [1,2,3] [10,20,30] [100,200,300])
-- 2
instance Applicative MyList where
  pure x = Nil
  (Nil)         <*> _             = Nil
  _             <*> (Nil)         = Nil
  (Cons f frem) <*> (Cons g grem) = Cons (($) f g) (frem <*> grem)

test_10_2 = (+) <$> mylst <*> mylst2
-- 3
myzipWith :: (a -> b -> c) -> MyList a -> MyList b -> MyList c
myzipWith f a b = (fmap f a) <*> b

myzipWith3 :: (a -> b -> c -> d) -> MyList a -> MyList b -> MyList c -> MyList d
myzipWith3 f a b c = ((fmap f a) <*> b) <*> c

prop_myzipwith ::Fun (Integer, Integer) Integer -> Bool
prop_myzipwith f = myzipWith (applyFun2 f) mylst mylst2 ==
                    fromList (zipWith (applyFun2 f) [1,2,3] [10,20,30])

prop_myzipwith3 :: Fun (Integer, Integer, Integer) Integer -> Bool
prop_myzipwith3 f = myzipWith3 (applyFun3 f) mylst mylst2 mylst3 ==
                    fromList (zipWith3 (applyFun3 f) [1,2,3] [10,20,30] [100,200,300])
------------------------------------------------------------------------------
-- Exercise 3-FP.11
------------------------------------------------------------------------------
getInt :: IO Integer
getInt = fmap (read::String->Integer) getLine

f :: IO Integer
f = ((+) <$> getInt) <*> getInt

------------------------------------------------------------------------------
-- Exercise 3-FP.12
------------------------------------------------------------------------------
justs :: [Maybe a] -> Maybe [a]
justs []     = Just []
justs (x:xs) = fmap (:) x <*> (justs xs)

test_12_all = test_12_1 && test_12_2 && test_12_3 && test_12_4 && test_12_5
test_12_1 = justs [Just 5, Just 3, Just 6] == Just [5,3,6]
test_12_2 = justs [Just 1, Just 2, Just 3] == Just [1,2,3]
test_12_3 = justs [Just 5, Nothing, Just 6] == Nothing
test_12_4 = justs [Nothing,Just 6] == Nothing
test_12_5 = justs [Just 5, Just 3, Nothing, Just 3, Just 8] == Nothing

------------------------------------------------------------------------------
-- Exercise 3-FP.13
------------------------------------------------------------------------------
data MyEndo a = MyEndo { apply :: a -> a}

instance Monoid (MyEndo a) where
  mempty = MyEndo (\x -> x)
  mappend (MyEndo f) (MyEndo g) = MyEndo (f . g)

-- 2
listtoendo :: [a -> a] -> MyEndo a
listtoendo fs = mconcat (map MyEndo fs)

transform :: String -> String
transform = apply $ listtoendo [map toUpper, reverse, filter isLetter]

transform' :: String -> String
transform' x = filter (isLetter) (reverse (map toUpper x))

prop_transform x = transform x == transform' x
------------------------------------------------------------------------------
-- Exercise 3-FP.14
------------------------------------------------------------------------------
data Parser r = P { runParser :: String -> [(r, String)]}

char :: Char -> Parser Char
char c = P p
  where p []          = []
        p (x:xs)
          | c == x    = [(x, xs)]
          | otherwise = []

-- 1
parseOne :: Parser Char
parseOne = char '1'

parserRun :: Parser r -> String -> [(r, String)]
parserRun p s = runParser p s

parseOneFail    = parserRun parseOne "2a2"
parseOneSatisfy = parserRun parseOne "1a21"
-- 2
instance Functor Parser where
  fmap f (P g) = P $ fmap (applyOnFirst f) . g

applyOnFirst :: (a -> b) -> (a, String) -> (b,String)
applyOnFirst f (x,y) = (f x, y)

-- 3
parseOneInt :: Parser Int
parseOneInt = fmap digitToInt parseOne

parseOneIntFail    = parserRun parseOneInt "2a2"
parseOneIntSatisfy = parserRun parseOneInt "1a21"

-- 4
instance Applicative Parser where
  pure f = P (\x -> [(f, x)])
  (P f) <*> (P g) = P (\s -> [(resultF resultG, s2) | (resultF, s1) <- f s, (resultG, s2) <- g s1])

parseAB :: Parser (Char, Char)
parseAB = (,) <$> char 'a' <*> char 'b'

parseABFail     = parserRun parseAB "acb"
parseABSatisfy  = parserRun parseAB "ab123b"

-- 5
parseString :: String -> Parser String
parseString "" = pure ""
parseString (x:xs) = (:) <$> char x <*> parseString xs

parserStringSatisfy = parserRun (parseString "ab") "ab123b"
parserStringFAil    = parserRun (parseString "abc") "ab123b"

------------------------------------------------------------------------------
-- Exercise 3-FP.15
------------------------------------------------------------------------------
fibonacci :: IO (Integer -> Integer)
fibonacci = (evalfun . parserFun) <$> readFile "fib.txt"

-- 2
fib5 :: IO Integer
fib5 = fibonacci <*> pure 5

fibs :: IO [Integer]
fibs = (map <$> fibonacci) <*> pure [0..]

-- Optional below
--fact = parserFun "function factorial x = if x == 0 then 1 else factorial(dec x) * x"
