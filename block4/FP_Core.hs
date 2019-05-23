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
type Variable = Int
type Stack  = [Int]
type Heap   = [Int]
data Op     = Add | Mul | Sub
            deriving (Show, Generic, ToRoseTree, Eq)


-- data Instr  = Push Int
--             | Calc Op
--             | EndProg
--             deriving (Show, Generic, ToRoseTree, Eq)

data Instr  = PushConst Int
            | PushAddr Int
            | Store Int
            | Calc Op
            | EndProg
            | PushPC
            | EndRep
            deriving (Show, Generic, ToRoseTree, Eq)

data Tick = Tick

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''
          | Var Variable                -- for variables
            deriving (Show, Generic, ToRoseTree)

data Stmnt = Assign Variable Expr
           | Repeat Expr [Stmnt]
             deriving (Show, Generic, ToRoseTree)
-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)

core :: [Instr] -> (Int,Int,Heap,Stack) -> Tick -> (Int,Int, Heap,Stack)

core instrs (pc,sp,heap,stack) tick =  case instrs!!pc of

        PushConst n   -> (pc+1, sp+1 , heap, stack <~ (sp,n))

        PushAddr n    -> (pc+1, sp+1, heap, stack <~ (sp, v))
                          where v = heap!!n

        Store n       -> (pc+1, sp-1, heap <~ (n, v), stack)
                          where v = stack!!(sp-1)

        Calc op       -> (pc+1, sp-1 , heap, stack <~ (sp-2,v))
                          where
                            v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        EndProg       -> (-1, sp, heap, stack)

        PushPC        -> (pc+1, sp+1, heap, stack <~ (sp, pc))

        EndRep        -> (newpc, sp-1, heap, stack <~ (sp-2, counter-1))
                          where counter = stack!!(sp-2)
                                newpc
                                  | (counter - 1) == 0 = pc + 1
                                  | otherwise          = stack!!(sp-1)

-- core :: [Instr] -> (Int,Int,Stack) -> Tick -> (Int,Int,Stack)
-- --
-- core instrs (pc,sp,stack) tick =  case instrs!!pc of
--
--         Push n   -> (pc+1, sp+1 , stack <~ (sp,n))
--
--         Calc op  -> (pc+1, sp-1 , stack <~ (sp-2,v))
--                  where
--                    v = alu op (stack!!(sp-2)) (stack!!(sp-1))
--
--         EndProg  -> (-1, sp, stack)

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
-- prog = [ Push 2
--        , Push 10
--        , Calc Mul
--        , Push 3
--        , Push 4
--        , Push 11
--        , Calc Add
--        , Calc Mul
--        , Calc Add
--        , Push 12
--        , Push 5
--        , Calc Add
--        , Calc Mul
--        , EndProg
--        ]

prog = [ PushConst 2
       , PushConst 10
       , Calc Mul
       , PushConst 3
       , PushConst 4
       , PushConst 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , PushConst 12
       , PushConst 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]

-- Testing
clock      = repeat Tick
emptyStack = replicate 8 0
emptyHeap = replicate 8 0

test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)

           $ scanl (core prog) (0,0,emptyHeap, emptyStack) clock

getResult :: [Instr] -> Int -> Int
getResult program sp = stack!!sp
  where coreResult   = takeWhile (\(pc,_,_,_) -> pc /= -1)
                     $ scanl (core program) (0,0,emptyHeap, emptyStack) clock
        (_,_,_,stack) = last coreResult
-- -- old test below :
-- test       = putStr
--           $ unlines
--           $ map show
--           $ takeWhile (\(pc,_,_,) -> pc /= -1)
--
--           $ scanl (core prog) (0,0, emptyStack) clock
