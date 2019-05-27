-- Imports
import Test.QuickCheck -- Ex 2, 3
import Data.Char -- Ex 4

-- Ex 1
f x = 2*x^2 + 3*x - 5

-- Ex 2
total1 :: Int -> Int
total1 0 = 0
total1 n = total1 (n-1) + n

total2 :: Int -> Int
-- total2 n = (n * (n+1)) `div` 2 -- Working version
total2 n = (n * (n+2)) `div` 2  -- Failing version

prop_total n = (n >= 0)  ==> total1 n == total2 n

-- Ex 3
prop_comm_add :: Int -> Int -> Bool
prop_comm_add x y  = x + y == y + x

prop_comm_sub :: Int -> Int -> Bool
prop_comm_sub x y  = x - y == y - x

-- Ex 4
code :: Char -> Char
code a
  | a >= 'A' && a < 'X'   = chr(ord a + 3)
  | a >= 'a' && a < 'x'   = chr(ord a + 3)
  | a >= 'A' && a <= 'z'  = chr(97 + ord a - 120 )
  | otherwise = a

-- Execute below function with map as: map (`code_gen` 3) "hello"
code_gen :: Char -> Int -> Char
code_gen a n
  | n >= 26                                 = code_gen a (n`mod`26)
  | n < 0                                   = code_gen a (26+n)
  | a >= 'A' && number_letter <=  ord 'Z'   = chr(number_letter)
  | a >= 'a' && number_letter <=  ord 'z'   = chr(number_letter)
  | all_letters && n > 0                    = chr(97 - 123 + number_letter )
  | otherwise = a
  where all_letters = (a >= 'A' && a <= 'Z') || (a >= 'a' && a <= 'z')
        number_letter = ord a + n


prop_code_eq3 :: Char -> Bool
prop_code_eq3 a = code a == code_gen a 3

prop_code_eq :: Char -> Int -> Bool
prop_code_eq a n = code_gen (code_gen a n) (26-n) == a

-- Ex 5
interest :: (Num a) => a -> a -> Int -> a
interest a r 0 = a
interest a r n = (1+r) * interest a r (n-1)

-- Ex 6
discr :: (Floating a, Num a) => a -> a -> a -> a
discr a b c = sqrt (b * b - 4 * a * c)
root1 :: (Fractional a, Floating a, Num a, RealFloat a) => a -> a -> a -> a
root1 a b c
  | isNaN (discr a b c)   = error "negative discriminant"
  | otherwise = (-b + discr a b c) / (2 * a)

root2 :: (Fractional a, Floating a, Num a, RealFloat a) => a -> a -> a -> a
root2 a b c
  | isNaN (discr a b c)   = error "negative discriminant"
  | otherwise = (-b - discr a b c) / (2 * a)

-- Ex 7
extrX :: (Fractional a, Num a) => a -> a -> a -> a
-- extrX 0 b c = error "assumption non-negative"
extrX a b c = (-b) / (2 * a)

extrY :: (Fractional a, Num a) => a -> a -> a -> a
extrY a b c =
  let x = extrX a b c
  in a*x^2 + b * x + c

-- Ex 8
mylength :: Num b => [a] -> b
mylength [] = 0
mylength (_:x) = mylength x + 1

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake 0 x = []
mytake n (x:xs) = mytake (n - 1) xs  ++ [x]

myelem :: Eq a => [a] -> a -> Bool
myelem [] y = False
myelem (x:xs) y = x == y || myelem xs y

myconcat :: [a] -> [a] -> [a]
myconcat [] [] = []
myconcat (x:xs) [] = myconcat xs [] ++ [x]
myconcat [] (y:ys) = myconcat [] ys ++ [y]
myconcat (x:xs) (y:ys) = myconcat xs ys ++ [x] ++ [y]

mymaximum :: Ord a => [a] -> a
mymaximum [] = error "no empty list max"
mymaximum [x] = x
mymaximum (x:xs)
  | x > curr_max = x
  | otherwise = curr_max
  where curr_max = mymaximum xs

myzip :: [a] -> [b] -> [(a,b)]
myzip [] a = []
myzip a [] = []
myzip (x:xs) (y:ys) = [(x, y)] ++ myzip xs ys
-- QuickCheck propositions
prop_mylength x = not (null x) ==> mylength x == length x
prop_mysum x = not (null x) ==> mysum x == sum x
prop_myreverse x = not (null x) ==> myreverse x == reverse x
prop_mytake n xs = n >= 0 ==> mytake n xs == take n xs
prop_myelem x y  = myelem x y == elem y x --elem is definid other way around
-- prop_myconcat x y = not (null x) and not (null y) ==> myconcat x y == concat x y
prop_mymaximum xs = not (null xs ) ==> mymaximum xs == maximum xs

-- Ex 9
r :: Num a => a -> a -> [a]
r a d = [a] ++ r (a+d) d
r1 :: Num a => a -> a -> Int -> a
r1 a d n = (r a d) !! n
totalr :: Int -> Int -> Int -> Int -> Int
totalr a d i j = sum [r1 a d i .. r1 a d j]

-- Ex 10
allEqual :: Eq a =>[a] -> Bool
allEqual [] = True
allEqual (x:x':xs) = x == x' && allEqual xs

isAS :: (Num a, Eq a) => [a] -> Bool
isAS [] = False
isAS [x] = True
isAS [x, x'] = True
isAS (x:x':x'':xs) =  (x-x') == (x' - x'') && isAS (x':x'':xs)

-- Ex 11
allRowsEquallyLong :: [[a]] -> Bool
allRowsEquallyLong x = allEqual(map length x)

rowTotals :: Num a => [[a]] -> [a]
rowTotals x = map sum x

myTranspose :: [[a]] -> [[a]]
myTranspose x
  | length (head x) == 0  = []
  | length (last x) == 0  = []
  | otherwise             = map head x : myTranspose (map tail x)

colTotals :: Num a => [[a]] -> [a]
colTotals x = rowTotals (myTranspose x)
