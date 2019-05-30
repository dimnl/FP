-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Dim Hoogeveen (s1728938)
-- Student 2: Other Name (syyyyyyy)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
-- import Tokenizer
import Test.QuickCheck
import Test.QuickCheck.All


------------------------------------------------------------------
--  D-5: FP3.1
------------------------------------------------------------------
-- Define datatypes for identifier and integer
type MicroIdent = String
type MicroInt   = Int

-- <program>
data MicroProgram  = MultipleFunction MicroFunction MicroProgram
                   | OneFunction MicroFunction
                   deriving Show
-- <function>
data MicroFunction = Function MicroIdent MicroArg MicroExpr
                   deriving Show

data MicroArg = Arg MicroExpr MicroArg
              | Empty
              deriving Show

-- <expr>, <term> and most of <factor>
data MicroExpr  = Add MicroExpr MicroExpr
                | Sub MicroExpr MicroExpr
                | Mult MicroExpr MicroExpr
                | Cons MicroInt
                | Var MicroIdent
                | If MicroCond MicroExpr MicroExpr
                | Paren MicroExpr
                | Exec MicroIdent MicroArg
                deriving Show

-- Condition of <factor>
data MicroCond = Cond MicroExpr MicroOrdering MicroExpr
               deriving Show

-- <ordering>
data MicroOrdering  = LessThan
                    | EqualTo
                    | GreaterThan
                    deriving Show

------------------------------------------------------------------
--  D-5: FP3.2
------------------------------------------------------------------
fibonacci = MultipleFunction (Function "fibonacci" (Arg (Cons 0) Empty) (Cons 0))
              (
                MultipleFunction (Function "fibonacci" (Arg (Cons 1) Empty) (Cons 1))
                  (
                    OneFunction (Function "fibonacci" (Arg (Var "n") Empty)
                      (Add
                        (Exec "fibonacci" (Arg (Sub (Var "n") (Cons 1)) Empty))
                        (Exec "fibonacci" (Arg (Sub (Var "n") (Cons 2)) Empty))
                      )
                    )
                  )
              )

fib = OneFunction (
        Function "fib" (Arg (Var "n") Empty)
        (If (Cond (Var "n") LessThan (Cons 3))
          (Cons 1) -- Cond True
          (Add     -- Cond False
          (Exec "fib" (Arg (Sub (Var "n") (Cons 1)) Empty))
          (Exec "fib" (Arg (Sub (Var "n") (Cons 2)) Empty))
          )
        )
      )

microsum = MultipleFunction (Function "sum" (Arg (Cons 0) Empty) (Cons 0))
        (
          OneFunction (Function "sum" (Arg (Var "a") Empty)
            (Add
              (Exec "sum" (Arg (Sub (Var "a") (Cons 1)) Empty))
              (Var "a")
            )
          )
        )

microdiv = OneFunction (Function "div" (Arg (Var "x") (Arg (Var "y") Empty))
              (If (Cond (Var "x") LessThan (Var "y"))
                (Cons 0) -- Cond True
                (Add     -- cond False
                  (Cons 1)
                  (Exec "div" (Arg (Sub (Var "x") (Var "y")) (Arg (Var "y") Empty)))
                )
              )
           )

twice = OneFunction (
          Function "twice" (Arg (Var "f") (Arg (Var "x") Empty))
          (Exec "f" (Arg (Exec "f" (Arg (Var "x") Empty)) Empty))
        )

add = OneFunction (Function "add" (Arg (Var "x") (Arg (Var "y") Empty))
        (Add (Var "x") (Var "y"))
      )

inc = OneFunction (Function "inc" (Arg (Var "x") Empty)
        (Exec "add" (Arg (Var "x") (Arg (Cons 1) Empty)))
      )

eleven = OneFunction (Function "inc" Empty
        (Exec "inc" (Arg (Cons 10) Empty))
      )

------------------------------------------------------------------
--  D-6: FP4.1
------------------------------------------------------------------
-- function :: Parser MicroFunction
-- function = Function <$>
-- factor :: Parser MicroExpr

-- term :: Parser MicroExpr
--
-- expr :: Parser MicroExpr

-- identifier :: Parser MicroIdent
-- identifier = P

-- Test using functions.txt

-- QuickCheck: all prop_* tests
-- return []
-- check = $quickCheckAll
