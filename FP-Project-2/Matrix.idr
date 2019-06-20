-- First of two main assignments for Idris as part of FP project 2
-- Student 1: Dim Hoogeveen (s1728938)

import Data.Vect

-- Definition Matrix
data Matrix : (n : Nat) -> (m : Nat) -> a -> Type where
  M : Vect n (Vect m a) -> Matrix n m a

--------------------------------------------------------------------------------
--  MAT1
--------------------------------------------------------------------------------
-- Define Functor instance for Matrix
Functor (Matrix n m) where
  map f (M xs) = M ( map (map f) xs)

-- Define some test matrices
mat1 : Matrix 2 3 Int
mat1 = M [[1,2,3],[4,5,6]]

mat2 : Matrix 5 2 Int
mat2 = M [[1,2],[3,4],[4,5],[6,7],[8,9]]

mat3 : Matrix 1 1 Int
mat3 = M [[1]]

-- Define cmult that multiplies a Matrix by a constant using Functor
cmult : Int -> Matrix n m Int -> Matrix n m Int
cmult c m = map (*c) m

-- Define an Applicative instance for Matrix
Applicative (Matrix n m) where
  pure x = M (replicate n (replicate m x))
  (<*>) (M f) (M x) = M ( zipWith (<*>) f x)

-- Define add that add two matrices element wise using Applicative
add : (Num a) => Matrix n m a -> Matrix n m a -> Matrix n m a
add x y = (+) <$> x <*> y

--------------------------------------------------------------------------------
--  MAT2
--------------------------------------------------------------------------------
-- Define transpose that transposes a Matrix recursively
transpose : Matrix n m a -> Matrix m n a
transpose {n = Z} {m} (M []) = M (replicate m [])
transpose {n = (S len)} (M (x :: xs)) = M (zipWith (::) x (transpose xs))

--------------------------------------------------------------------------------
--  MAT3
--------------------------------------------------------------------------------
-- Define eye that creates the identity matrix of size n recursively
eye : (Num a) => (n : Nat) -> Matrix n n a
eye Z = M []
eye (S n) with (eye n)
  | (M xs) = M (zipWith (::) (1::zeroes) (insertAt FZ zeroes xs))
    where zeroes = replicate n 0
