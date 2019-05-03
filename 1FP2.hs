-- Imports
import Test.QuickCheck
import Data.Char
import Data.List

-- Ex 12
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs)
  | f x       = x : myfilter f xs
  | otherwise = myfilter f xs

myfoldl ::(a -> b -> a) -> a -> [b] -> a
myfoldl f a []      = a
myfoldl f a (x:xs)  = myfoldl f (a `f` x) xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f a []      = a
myfoldr f a xs  = myfoldr f ((last xs) `f` a) (init xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith _ [] _  = []
myzipWith _ _ []  = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys

--  Ex 13
type Name = String
type Age = Int
type Sex = String
type Place = String
type Person = (Name,Age,Sex,Place)

person :: [Person]
person = [("a", 1, "M", "Enschede"), ("b", 5, "M", "Zwolle"),("c", 34, "F", "Hengelo"), ("d", 11, "F", "Enschede")]
getName :: Person -> Name
getName (a,_,_,_)   = a
getAge :: Person -> Age
getAge (_,b,_,_)    = b
getSex :: Person -> Sex
getSex (_,_,c,_)    = c
getPlace :: Person -> Place
getPlace (_,_,_,d)  = d

ageRecursion :: [Person] -> Int -> [Person]
ageRecursion [] n = []
ageRecursion (x:xs) n = (getName x, getAge x + n, getSex x, getPlace x) : ageRecursion xs n

ageList ::[Person] -> Int -> [Person]
ageList xs n = [(getName x, getAge x + n, getSex x, getPlace x) | x <- xs]

ageHighOrder :: [Person] -> Int -> [Person]
ageHighOrder xs n = map (incAge n) xs

incAge :: Int -> (Person -> Person)
incAge n (a, b, c, d) = (a, b + n, c, d)

isF3040 :: Person -> Bool
isF3040 (_,b,c,_)
  | b >= 30 && b <= 40 && (c == "F" || c == "f")  = True
  | otherwise                                     = False

f3040Rec :: [Person] -> [Person]
f3040Rec [] = []
f3040Rec (x:xs)
  | isF3040 x = x : f3040Rec xs
  | otherwise = f3040Rec xs

f3040List :: [Person] -> [Person]
f3040List xs = [ (getName x, getAge x, getSex x, getPlace x) | x <- xs, isF3040 x]

f3040HOF :: [Person] -> [Person]
f3040HOF xs = filter (isF3040) xs

getAgeByName :: Name-> [Person] -> Age
getAgeByName _ []     = error "no person with this name"
getAgeByName name (x:xs)
  | map toLower (getName x) == map toLower name = getAge x
  | otherwise                                   = getAgeByName name xs

sortByAge :: [Person] -> [Person]
sortByAge x = map (\(a,b,c,d)-> (b,a,c,d)) (sort (map (\(a,b,c,d)-> (b,a,c,d)) x))

-- Ex 14
sieve :: [Int] -> [Int]
sieve []      = []
sieve (x:xs)  = x:sieve(filter (\y-> y `mod` x /= 0) xs)

isPrime :: Int -> Bool
isPrime x = elem x (sieve [2..x])

getNPrimes :: Int -> [Int]
getNPrimes n = take n (sieve [2..])

allPrimesSmallerThan :: Int -> [Int]
allPrimesSmallerThan n = sieve [2..n]

dividers :: Int -> [Int]
dividers m = [x | x <- [1..m], m `mod` x == 0]

-- Ex 15
pyth :: Int -> [(Int, Int, Int)]
pyth n = [(x,y,z)| x <- [1..n-1], y <-[1..n-1], z <-[1..n-1], x*x+y*y == z*z]

-- Ex 16
increasing :: (Ord a,Num a) =>[a] -> Bool
increasing [x] = True
increasing (x:x':xs) = x < x' && increasing (x':xs)

weaklyIncreasing :: (Fractional a, Num a, Ord a, Enum a) => [a] -> Bool
weaklyIncreasing [x] = True
-- Not correct; greater than mean of last numbers
weaklyIncreasing x = increasing (zipWith (/) (scanl (+) 1 x) [1..])

-- Ex 17
sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist _ [] = False
sublist xs@(x1:xr) ys@(y1:yr)
  | xs == ys = True
  | x1 == y1 = sublist xr (take (length xs) ys)
  | otherwise = sublist xs yr

partSublist :: Eq a => [a] -> [a] -> Bool
partSublist [] _ = True
partSublist _ [] = False
partSublist allx@(x:xs) (y:ys)
  | x == y  = partSublist xs ys
  | otherwise = partSublist allx ys

-- Ex 18
bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort [x] = [x]
bsort x
  | (last x) == maximum x = bsort (bubble (init x)) ++ [last x]
  | otherwise = bsort (bubble x)

bubble :: Ord a => [a] -> [a]
bubble [x] = [x]
bubble (x:x':xs)
  | x > x'    = x': bubble (x:xs)
  | otherwise = x: bubble (x':xs)

prop_bs :: [Int] -> Bool
prop_bs x = bsort x == sort x
-- 2
mmsort :: Ord a => [a] -> [a]
mmsort [] = []
mmsort [x] = [x]
mmsort x =
    let minX = [minimum x]
        maxX = [maximum x]
    in minX ++ mmsort ((x \\ minX) \\ maxX) ++ maxX
-- 3
isort :: Ord a => [a] -> [a]
isort x = foldr ins [] x

ins:: Ord a => a -> [a] -> [a]
ins x []     = [x]
ins x (y:ys) = min x y : ins (max x y) ys
-- 4
merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
  | x < y     = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort x = merge (msort left) (msort right)
  where left  = take (length x `div` 2) x
        right = drop (length x `div` 2) x
-- 5
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [s | s <- xs, s <= x] ++ [x] ++ qsort [b | b <- xs, b > x]

-- Ex 19
myflip :: (a -> b -> c) -> (b -> a -> c)
-- myflip f x y = f y x -- for test
myflip f = \x y -> f y x
-- Lambda notation to show it is mainly for making a new funciton and pass it to a map or filter

-- Ex 20
transform :: String -> String
-- transform x = filter (isLetter) (reverse (map toUpper x))--first working version
transform = reverse . filter isLetter $ map toUpper
