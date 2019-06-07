-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Dim Hoogeveen (s1728938)
-- Student 2: Other Name (syyyyyyy)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
-- import Test.QuickCheck
-- import Test.QuickCheck.All


------------------------------------------------------------------
--  D-5: FP3.1
------------------------------------------------------------------
-- Define datatypes for identifier and integer
type MicroIdent = String
type MicroInt   = Integer

-- <program>
data MicroProgram  = MultipleFunction MicroFunction MicroProgram
                   | OneFunction MicroFunction
                   deriving (Show, Eq)
-- <function>
data MicroFunction = Function MicroIdent MicroArg MicroExpr
                   deriving (Show, Eq)

data MicroArg = Arg MicroExpr MicroArg
              | Empty
              deriving (Show, Eq)

-- <expr>, <term> and most of <factor>
data MicroExpr  = Add MicroExpr MicroExpr
                | Sub MicroExpr MicroExpr
                | Mult MicroExpr MicroExpr
                | Cons MicroInt
                | Var MicroIdent
                | If MicroCond MicroExpr MicroExpr
                | Paren MicroExpr
                | Exec MicroIdent MicroArg
                deriving (Show, Eq)

-- Condition of <factor>
data MicroCond = Cond MicroExpr MicroOrdering MicroExpr
               deriving (Show, Eq)

-- <ordering>
data MicroOrdering  = LessThan
                    | EqualTo
                    | GreaterThan
                    deriving (Show, Eq)

------------------------------------------------------------------
--  D-5: FP3.2
------------------------------------------------------------------
-- fibonacci of functions.txt
fibonacci :: MicroProgram
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

-- fib of functions.txt
fib :: MicroProgram
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

-- sum of functions.txt
microsum :: MicroProgram
microsum = MultipleFunction (Function "sum" (Arg (Cons 0) Empty) (Cons 0))
        (
          OneFunction (Function "sum" (Arg (Var "a") Empty)
            (Add
              (Exec "sum" (Arg (Sub (Var "a") (Cons 1)) Empty))
              (Var "a")
            )
          )
        )

-- div of funcitons.txt
microdiv :: MicroProgram
microdiv = OneFunction (Function "div" (Arg (Var "x") (Arg (Var "y") Empty))
              (If (Cond (Var "x") LessThan (Var "y"))
                (Cons 0) -- Cond True
                (Add     -- cond False
                  (Cons 1)
                  (Exec "div" (Arg (Paren(Sub (Var "x") (Var "y"))) (Arg (Var "y") Empty)))
                )
              )
           )

-- twice of functions.txt
twice :: MicroProgram
twice = OneFunction (
          Function "twice" (Arg (Var "f") (Arg (Var "x") Empty))
          (Exec "f" (Arg (Exec "f" (Arg (Var "x") Empty)) Empty))
        )

-- add of functions.txt
add :: MicroProgram
add = OneFunction (Function "add" (Arg (Var "x") (Arg (Var "y") Empty))
        (Add (Var "x") (Var "y"))
      )

-- Helper function to retrieve function from program
getLastFunctionFromProgram :: MicroProgram -> MicroFunction
getLastFunctionFromProgram (MultipleFunction func prog) = getLastFunctionFromProgram prog
getLastFunctionFromProgram (OneFunction func) = func

-- inc of funcitons.txt
inc :: MicroProgram
inc = MultipleFunction (addF) (OneFunction (Function "inc" Empty
        (Exec "add" (Arg (Cons 1) Empty))
      ))
      where addF =  getLastFunctionFromProgram add

-- eleven of functions.txt
eleven :: MicroProgram
eleven = MultipleFunction (addF) (
          MultipleFunction (incF) (
            OneFunction (Function "eleven" Empty
                    (Exec "inc" (Arg (Cons 10) Empty))
                        )
          ))
      where addF = getLastFunctionFromProgram add
            incF = getLastFunctionFromProgram inc

------------------------------------------------------------------
--  D-5: FP3.3
------------------------------------------------------------------
-- Pretty printer to IO ()
printPretty :: (PrettyPrint a) => a -> IO ()
printPretty = putStr . pretty

-- Class with instances to define function for pretty printing
class PrettyPrint a where
  pretty :: a -> String

instance PrettyPrint MicroProgram where
  pretty (MultipleFunction func prog) = pretty func ++ "\n" ++ pretty prog
  pretty (OneFunction func) = pretty func ++ "\n"

instance PrettyPrint MicroFunction where
  pretty (Function ident arg expr) = ident ++ " " ++ prettyArgSpaces arg ++ " := " ++ pretty expr ++ ";"

prettyArgSpaces :: MicroArg -> String
prettyArgSpaces args = case args of
  (Arg expr Empty)  -> pretty expr
  (Arg expr arg)    -> pretty expr ++ " " ++ prettyArgSpaces arg
  Empty             -> ""

instance PrettyPrint MicroArg where
  pretty args = case args of
    (Arg expr Empty) -> pretty expr
    (Arg expr arg)   -> pretty expr ++ ", " ++ pretty arg
    Empty            -> ""

instance PrettyPrint MicroExpr where
  pretty expres = case expres of
    (Add expr1 expr2)     -> pretty expr1 ++ " + " ++ pretty expr2
    (Sub expr1 expr2)     -> pretty expr1 ++ " - " ++ pretty expr2
    (Cons int )           -> show int
    (Var ident)           -> ident
    (If cond expr1 expr2) -> ("if " ++ pretty cond ++
                                  " then { " ++ pretty expr1 ++
                                  " } else { " ++ pretty expr2 ++ " }")
    (Paren expr)          -> "(" ++ pretty expr ++ ")"
    (Exec ident arg)      -> ident ++ " (" ++ pretty arg ++ ")"

instance PrettyPrint MicroCond where
  pretty (Cond expr1 order expr2) = "(" ++ pretty expr1 ++ pretty order ++ pretty expr2 ++ ")"

instance PrettyPrint MicroOrdering where
  pretty op = case op of
    LessThan    -> " < "
    EqualTo     -> " == "
    GreaterThan -> " > "

------------------------------------------------------------------
--  D-5: FP3.4
------------------------------------------------------------------
-- Evauluates MicroProgram and returns answer of EDSL
eval :: MicroProgram -> [(MicroIdent, MicroInt)] -> MicroInt
eval (MultipleFunction func prog) vals = eval prog vals -- only evaluate last function
eval (OneFunction func) vals = evalFun func vals

-- Evauluates MicroFunction and returns answer of EDSL
evalFun :: MicroFunction -> [(MicroIdent, MicroInt)] -> MicroInt
evalFun func@(Function _ _ expr) vals = evalExpr func expr vals

-- Evauluates MicroExpr and returns answer of EDSL
evalExpr :: MicroFunction -> MicroExpr -> [(MicroIdent, MicroInt)] -> MicroInt
evalExpr func@(Function _ args _) expr vals = case expr of
  (Add  x y) -> evalExpr func x vals + evalExpr func y vals
  (Sub  x y) -> evalExpr func x vals - evalExpr func y vals
  (Mult x y) -> evalExpr func x vals * evalExpr func y vals
  (Cons x)   -> x
  (Var x)    -> case filter ((==x).fst) vals of
                  [] -> error ("Value unkonwn of variable: " ++ x)
                  v  -> snd $ head  v
  (If c x y) | evalCond func c vals -> evalExpr func x vals
             | otherwise            -> evalExpr func y vals
  (Paren x)  -> evalExpr func x vals
  (Exec i a) -> evalFun func (zip (evalFunctionArgs args) (evalArgs func a vals))

-- Evauluates MicroCond and returns Bool
evalCond :: MicroFunction -> MicroCond -> [(MicroIdent, MicroInt)] -> Bool
evalCond func (Cond x o y) vals = order c1 c2
  where c1    = evalExpr func x vals
        order = evalOrd o
        c2    = evalExpr func y vals

-- Evauluates MicroOrdering and returns answer of EDSL
evalOrd :: (Ord a) => MicroOrdering -> a -> a -> Bool
evalOrd op = case op of
              LessThan -> (<)
              EqualTo -> (==)
              GreaterThan -> (>)

-- Evauluates input arguments of MicroFunction and returns list of MicroIdent
evalFunctionArgs :: MicroArg -> [MicroIdent]
evalFunctionArgs args = case args of
  (Arg (Var ident) Empty) -> [ident]
  (Arg (Cons integ) Empty) -> [show integ]
  (Arg expr arg) -> evalFunctionArgs (Arg expr Empty) ++ evalFunctionArgs arg
  Empty -> []

-- Evauluates input argumetns of Exec of MicroExpr and returns list of MicroInt
evalArgs :: MicroFunction -> MicroArg -> [(MicroIdent, MicroInt)] -> [MicroInt]
evalArgs func args vals = case args of
  (Arg expr Empty) -> [evalExpr func expr vals]
  (Arg expr arg)   -> [evalExpr func expr vals] ++ evalArgs func arg vals
  Empty            -> []

-- Simple tests
evalAdd x y = eval add [("x",x), ("y",y)]
evalAdd1 = evalAdd 4 5 == 4 + 5

evalMath s = eval (compile ("f := " ++ s ++ ";")) []
evalMath1 = evalMath "(1+2)*3" == (1+2)*3
evalMath2 = evalMath "2-(3+(1+2))*3" ==2-(3+(1+2))*3

evalFib n = eval fib [("n",n)]
evalFib8 = evalFib 8 == 21

evalDiv x y = eval microdiv [("x", x),("y",y)]
evalDiv1 = evalDiv 123 3 == div 123 3

testAllEval = evalAdd1 && evalMath1 && evalMath2 && evalFib8 && evalDiv1

------------------------------------------------------------------
--  D-6: FP4.1
------------------------------------------------------------------
-- Parse program :
-- (<function>)+
program :: Parser MicroProgram
program =  MultipleFunction <$> function <*> program
       <|> OneFunction <$> function

-- Parse function :
-- identifier(identifier|integer)* ':='' <expr> ';'
function :: Parser MicroFunction
function = Function <$> identifier <*> functionArg <*> (symbol ":=" *> expr) <* symbol ";"

-- Parse input arguments of MicroFunction
functionArg :: Parser MicroArg
functionArg =  (Arg <$> (ident<|>integ) <*> option Empty functionArg)
            <|> pure Empty
      where   ident = Var <$> identifier
              integ = Cons <$> integer

-- Parse expression :
--  <term>|<term>('+'|'-')<expr>
expr :: Parser MicroExpr
expr =  Add <$> (term <* symbol "+") <*> expr
    <|> Sub <$> (term <* symbol "-") <*> expr
    <|> term

-- Parse factor :
-- identifier ('(' <expr>(',' <expr>)* ')')?
-- 'if' '(' <condition> ')' 'then' '{' <expr> '}' 'else' '{' <expr> '}'
-- '(' <expr> ')'
-- integer
factor :: Parser MicroExpr
factor =  Exec <$> identifier <*> (whitespace $ parens args)
      <|> If <$> (symbol "if" *> (whitespace $ parens condition))
             <*> (symbol "then" *> (whitespace $ braces expr))
             <*> (symbol "else" *> (whitespace $ braces expr))
      <|> Paren <$> parens expr
      <|> Cons <$> integer
      <|> Var <$> identifier

-- Parse term :
--  <factor>|<factor>'*'<term>
term :: Parser MicroExpr
term = Mult <$> (factor <* symbol "*") <*> term
     <|> factor

-- Parse arguments :
-- ('(' <expr> (',' <expr>)* ')')?
args :: Parser MicroArg
args = Arg <$> expr <*> option Empty (char ',' *> args)
     <|> pure Empty

-- Parse ordering :
-- '<' | '==' | '>'
ordering :: Parser MicroOrdering
ordering =  symbol "<" *> pure LessThan
        <|> symbol ">" *> pure GreaterThan
        <|> symbol "==" *> pure EqualTo

-- Parse condition :
-- <expr> <ordering> <expr>
condition :: Parser MicroCond
condition = Cond <$> expr <*> ordering <*> expr

-- Helper function for testing
runProgram :: String -> [(MicroProgram, Stream)]
runProgram s = runParser program (Stream s)

-- Helper function for testing
getParsed :: [(a, Stream)] -> a
getParsed = fst . head

-- Simple tests (using defined functions of FP3.2)
prop_parseFibonacci = fibonacci == compile (pretty fibonacci)
prop_parseFib = fib == compile (pretty fib)
prop_parseDiv = microdiv == compile (pretty microdiv)
prop_parseTwice = twice == compile (pretty twice)
prop_parseAdd = add == compile (pretty add)
prop_parseInc = inc == compile (pretty inc)
prop_parseEleven = eleven == compile (pretty eleven)

prop_allFunctions = prop_parseFibonacci && prop_parseFib && prop_parseDiv
  && prop_parseTwice && prop_parseAdd && prop_parseInc && prop_parseEleven

------------------------------------------------------------------
--  D-6: FP4.2
------------------------------------------------------------------
-- Tokenizes and compiles a representation of MicroProgram
compile :: String -> MicroProgram
compile = getParsed . runProgram

-- Small test using "functions.txt"
testCompileFunctions = compile <$> readFile "functions.txt"
