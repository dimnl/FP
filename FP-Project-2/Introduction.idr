-- Introduction assignments for Idris as part of FP project 2
-- Student 1: Dim Hoogeveen (s1728938)

import Data.Vect

--------------------------------------------------------------------------------
--  IDR1
--------------------------------------------------------------------------------
-- 1. return first element  for vectors with length > 0
myhead : Vect (S len) elem -> elem
myhead (x :: xs) = x

-- 2. return all elements except first for vectors with length > 0
mytail : Vect (S len) elem -> Vect len elem
mytail (x :: xs) = xs

-- 3. return first n elements of vector
mytake : (n : Nat) -> Vect (n + len) elem -> Vect n elem
mytake Z _  = []
mytake (S n) (x :: xs) = x :: mytake n xs

-- 4. return concatenated vector from a vector of vectors
myconcat : Vect n (Vect m elem) -> Vect (n*m) elem
myconcat [] = []
myconcat (x :: xs) = x ++ myconcat xs

-- 5. return vector with element inserted at specified indexa
myinsert : (n : Nat) -> elem -> Vect (n + len) elem -> Vect (S (n + len)) elem
myinsert Z e xs = e :: xs
myinsert (S k) e (x :: xs) = x :: myinsert k e xs

--------------------------------------------------------------------------------
--  IDR2
--------------------------------------------------------------------------------
-- 1. add type definition
myzip : Vect n a -> Vect n b -> Vect n (a,b)

-- 2. add-clause when selecting myzipi
-- myzip xs ys = ?myzip_rhs

-- 3. case-split xs, then case-split ys
-- myzip [] [] = ?myzip_rhs_3
-- myzip (x :: xs) (y :: ys) = ?myzip_rhs_1

-- 4. proof-serach on both holes
myzip [] [] = []
myzip (x :: xs) (y :: ys) = (x, y) :: myzip xs ys

-- 5. check if works
prop_myzip : Bool
prop_myzip = myzip (fromList [1,2]) (fromList [3,4]) == fromList [(1,3), (2,4)]
