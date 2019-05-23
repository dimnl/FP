import FP_Core


------------------------------------------------------------------------------
--Exercise 4-FP1
------------------------------------------------------------------------------
-- test
-- showRoseTree $ toRoseTree expr

------------------------------------------------------------------------------
--Exercise 4-FP2
------------------------------------------------------------------------------
-- Exercise commented out, because of changes on FP_Core
--
-- codeGen2 :: Expr -> [Instr]
-- codeGen2 (Const x) = [Push x] ++ [EndProg]
-- codeGen2 (BinExpr op e1 e2) = codeGenHelp e1 ++ codeGenHelp e2 ++ [Calc op] ++ [EndProg]
--
-- codeGenHelp :: Expr -> [Instr]
-- codeGenHelp (Const x) = [Push x]
-- codeGenHelp (BinExpr op e1 e2) = codeGenHelp e1 ++ codeGenHelp e2 ++ [Calc op]
--
-- test2 = codeGen expr == prog

------------------------------------------------------------------------------
--Exercise 4-FP3
------------------------------------------------------------------------------

-- See FP_Core

------------------------------------------------------------------------------
--Exercise 4-FP4
------------------------------------------------------------------------------
-- 1 -> See FP_Core

-- 2
codeGen4 :: Stmnt -> [Instr]
-- codeGen4 (Assign loc expr) = [Store val, PushAddr loc]
--   where val  = getResult $ codeGen4' expr
codeGen4 (Assign loc expr) = codeGenHelp expr ++ [Store loc]
-- Added for FP6:
codeGen4 (Repeat expr stmnt) = pushCounter
                              ++ [PushPC]
                              ++ foldl (++) [] (map codeGen4 stmnt)
                              ++ [EndRep]
  where pushCounter = codeGenHelp $ Const (getResult (codeGen4' expr) 0)

-- below is modified from 4-FP2
codeGen4' :: Expr -> [Instr]
codeGen4' (Const x) = [PushConst x] ++ [EndProg]
codeGen4' (BinExpr op e1 e2) = codeGenHelp e1 ++ codeGenHelp e2 ++ [Calc op] ++ [EndProg]

codeGenHelp :: Expr -> [Instr]
codeGenHelp (Const x) = [PushConst x]
codeGenHelp (BinExpr op e1 e2) = codeGenHelp e1 ++ codeGenHelp e2 ++ [Calc op]
codeGenHelp (Var v) = [PushAddr v]

test4 = codeGen4 (Assign 2 (BinExpr Add (Const 1) (Const 2)))

------------------------------------------------------------------------------
--Exercise 4-FP5
------------------------------------------------------------------------------
class CodeGen x where
  codeGen :: x -> [Instr]

instance CodeGen Expr where
  codeGen = codeGen2

instance CodeGen Stmnt where
  codeGen = codeGen4

-- Changed version of FP2 (Push changed to PushConst)
codeGen2 :: Expr -> [Instr]
codeGen2 (Const x) = [PushConst x] ++ [EndProg]
codeGen2 (BinExpr op e1 e2) = codeGenHelp e1 ++ codeGenHelp e2 ++ [Calc op] ++ [EndProg]

------------------------------------------------------------------------------
--Exercise 4-FP6
------------------------------------------------------------------------------
-- See FP_Core & FP4

------------------------------------------------------------------------------
--Exercise 4-FP7
------------------------------------------------------------------------------
stmnt = Repeat (Const 10)
          [
            Assign 2 (BinExpr Add (Var 2) (Const 1))
            ,Assign 1 (BinExpr Add (Var 1) (Var 2))

          ]

pr = codeGen stmnt ++ [EndProg]

check_test7 = getResult pr 2 == 10+9+8+7+6+5+4+3+2+1

test7      = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)

           $ scanl (core pr) (0,0,emptyHeap, emptyStack) clock
