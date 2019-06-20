-- First of two main assignments for Idris as part of FP project 2
-- Student 1: Dim Hoogeveen (s1728938)

import Data.Vect

--------------------------------------------------------------------------------
--  SEL1
--------------------------------------------------------------------------------
-- Count number of values that are True in given Vect recursively
idxsize : Vect n Bool -> Nat
idxsize [] = 0
idxsize (True :: xs)  = 1 + idxsize xs
idxsize (False :: xs) = idxsize xs

--------------------------------------------------------------------------------
--  SEL2
--------------------------------------------------------------------------------
-- Combine Vects pairwise recursively
-- by only returning element of second Vect if element of first Vect is True
get : (xs : Vect n Bool) -> Vect n a -> Vect (idxsize xs) a
get [] [] = []
get (True :: xs) (y :: ys) = y :: get xs ys
get (False :: xs) (y :: ys)= get xs ys

prop_get : Bool
prop_get = get [True,False,True] [1,2,3] == [1,3]

--------------------------------------------------------------------------------
--  SEL3
--------------------------------------------------------------------------------
-- Combine Vects recursively in resulting Vect
-- inserts element of third Vect if element of first Vect is True
-- otherwise inserts element of second Vect at same index as first Vect
put : (xs : Vect n Bool) -> Vect n a -> Vect (idxsize xs) a -> Vect n a
put [] [] [] = []
put (True :: xs) (y :: ys) (z :: zs) = z :: put xs ys zs
put (False :: xs) (y :: ys) zs       = y :: put xs ys zs

prop_put : Bool
prop_put = put [True,False,True] [1,2,3] [5,6] == [5,2,6]
--------------------------------------------------------------------------------
--  SEL4
--------------------------------------------------------------------------------
-- Apply a given function to the indexed values that are True
app : (xs : Vect n Bool) -> (Vect (idxsize xs) a ->  Vect (idxsize xs) a) -> Vect n a -> Vect n a
app [] f [] = []
app xs f ys = put xs ys (f (get xs ys))

prop_app : Bool
prop_app = app [True,False,True,True,True] reverse [1,2,3,4,5] == [5,2,4,3,1]
