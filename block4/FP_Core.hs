{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module FP_Core where

import GHC.Generics
import FPPrac.Trees

{-
Extension of CoreIntro.hs:
- instructions as program *in* the processor,
- stack now is list of fixed length,i
- clock added
- program counter + stack pointer added,
- instruction EndProg added,
- update operaton (<~) added,
-}

-- ========================================================================

type Stack  = [Int]

data Op     = Add | Mul | Sub
            deriving (Show, Generic, ToRoseTree)


data Instr  = Push Int
            | Calc Op
            | EndProg
            deriving (Show, Generic, ToRoseTree)


data Tick = Tick

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''
            deriving (Show, Generic, ToRoseTree)

-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)


core :: [Instr] -> (Int,Int,Stack) -> Tick -> (Int,Int,Stack)

core instrs (pc,sp,stack) tick =  case instrs!!pc of

        Push n   -> (pc+1, sp+1 , stack <~ (sp,n))

        Calc op  -> (pc+1, sp-1 , stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        EndProg  -> (-1, sp, stack)

-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

-- The program that results in the value of the expression (1105):
prog = [ Push 2
       , Push 10
       , Calc Mul
       , Push 3
       , Push 4
       , Push 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , Push 12
       , Push 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]

-- Testing
clock      = repeat Tick
emptyStack = replicate 8 0
test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_) -> pc /= -1)

           $ scanl (core prog) (0,0,emptyStack) clock


