-- First of two main assignments for Idris as part of FP project 2
-- Student 1: Dim Hoogeveen (s1728938)

import Data.Vect

--------------------------------------------------------------------------------
--  SEL1
--------------------------------------------------------------------------------
-- Count number of values that are True in given Vect recursively
idxsize : Vect n Bool -> Nat
idxsize [] = 0
idxsize (True :: xs) = 1 + idxsize xs
idxsize (False :: xs) = idxsize xs

--------------------------------------------------------------------------------
--  SEL2
--------------------------------------------------------------------------------
-- Combine Vects pairwise recursively
-- by only returning element of second Vect if element of first Vect is True
get : (xs : Vect n Bool) -> Vect n a -> (p ** Vect p a)
get {n=Z} _ _ = (_**[])
get (x :: xs) (y :: ys) with (get xs ys)
  | (_**xs') = if x == True then (_**y :: xs') else (_**xs')

--Preferably change p ** Vect p a to Vect len a; and redefine n to n+len
-- Old version (using m of Vect m a as ???)
-- get {m = Z} [] [] = []
-- get {m = (S len)} (x :: xs) (y :: ys) = if x == True then y :: get xs ys
--                                        else get xs ys

prop_get : Bool
prop_get = snd (get [True,False,True] [1,2,3]) == [1,3]

--------------------------------------------------------------------------------
--  SEL3
--------------------------------------------------------------------------------
-- Combine Vects recursively in resulting Vect
-- inserts element of third Vect if element of first Vect is True
-- otherwise inserts element of second Vect at same index as first Vect
put : (xs : Vect n Bool) -> Vect n a -> (p ** Vect p a) -> Vect n a
put [] [] (_**[]) = []
put (x :: xs) (y :: ys) (_**(z :: zs)) =
  if x == True then z :: put xs ys (_**zs)
               else y :: put xs ys (_**(z :: zs))

prop_put : Bool
prop_put = put [True,False,True] [1,2,3] (2**[5,6]) == [5,2,6]
--------------------------------------------------------------------------------
--  SEL4
--------------------------------------------------------------------------------
-- Apply a given function to the indexed values that are True
app : (xs : Vect n Bool) -> ((p ** Vect p a) -> (p ** Vect p a)) -> Vect n a -> Vect n a
app [] f [] = []
app xs f ys = put xs ys (f (get xs ys))

-- prop_app : Bool
-- prop_app = app [True,False,True,True,True] reverse [1,2,3,4,5] == [5,2,4,3,1]
