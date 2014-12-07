module LISCompilation (
--     compileProgram,
    compileNExp,
    compileBExp
) where

import AssemblyRepresentation
import LISRepresentation
import StateMonad

type Memory = [Label]

-- compileProgram :: Program -> AssemblyProgram
-- compileProgram (Program bl) = AssemblyProgram (evalState (compileBlock bl) startState)

startState :: Memory
startState = []

-- compileBlock :: Block -> State Memory [Mnemonic]
-- compileBlock bl =

-- compileComm :: Command -> State Memory [Mnemonic]
-- compileComm Skip = return [NoOp]
-- compileComm (Assign var nExp) = State $ \s -> (var:s, [Store A v, compileNExp nExp])
-- compileComm (If bExp trueBlock falseBlock) =
-- compileComm (While bExp block) =

doStackOp :: [Mnemonic] -> State Memory [Mnemonic]
doStackOp mnemonics = return (mnemonics ++ [Push A])

compileBinExp compile exp1 exp2 assemblyOp =
  do  ops1 <- compile exp1
      ops2 <- compile exp2
      doStackOp $ ops1 ++ [Pop B] ++ ops2 ++ [Pop A, assemblyOp A B]

compileBinNExp = compileBinExp compileNExp

compileNExp :: NExp -> State Memory [Mnemonic]
compileNExp (Vble x) = state $ \s -> ([], x:s)
compileNExp (NCte n) = doStackOp [Load A n]
compileNExp (Add exp1 exp2) = compileBinNExp exp1 exp2 ADD
compileNExp (Sub exp1 exp2) = compileBinNExp exp1 exp2 SUB
compileNExp (Div exp1 exp2) = compileBinNExp exp1 exp2 DIV
compileNExp (Mod exp1 exp2) = compileBinNExp exp1 exp2 MOD

compileBinBExp = compileBinExp compileBExp

compileBExp :: BExp -> State Memory [Mnemonic]
compileBExp (BCte b) = compileNExp (NCte (delta b))
compileBExp (Not exp) =
  do  ops <- compileBExp exp
      doStackOp $ ops ++ [Pop A, Load B 1, ADDmod2 A B]

compileBExp (And exp1 exp2) = compileBinBExp exp1 exp2 MUL
compileBExp (Or exp1 exp2) = compileBExp (Not (And exp1 exp2))

-- compileBExp (Cmp op exp1 exp2) =
