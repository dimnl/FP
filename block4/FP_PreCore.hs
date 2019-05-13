module FP_PreCore where

{-
Elementary concept of processor (represented by function f below; in fact, function g is alu);
An instruction is considered as "input", i.e., instructions are not in "memory"
-}

data Op    = Add | Mul | Sub
           deriving Show

data Instr = Push Int
           | Calc Op
           deriving Show

-- ========================================================================
-- Processor functions

g op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)

f stack instr = case instr of

                     Push n   -> n : stack

                     Calc op  -> v : drop 2 stack
                              where
                                v = g op (stack!!1) (stack!!0)

-- ========================================================================
-- example Program for expression: (((5*10) - (3*(4+6))) * (2+1))

prog = [ Push 5
       , Push 10
       , Calc Mul
       , Push 3
       , Push 4
       , Push 6
       , Calc Add
       , Calc Mul
       , Calc Sub
       , Push 2
       , Push 1
       , Calc Add
       , Calc Mul
       ]

test0 = foldl f [] prog

test1 = scanl f [] prog

test2 = putStr $ unlines $ map show
      $ scanl f [] prog

