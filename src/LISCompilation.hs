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

-- compileBinExp :: (NExp -> State Memory [Mnemonic]) -> NExp -> NExp -> (Reg -> Reg -> Mnemonic) -> State Memory [Mnemonic]
-- compileBinExp compile exp1 exp2 assemblyOp = doStackOp $ compile exp2 ++ [Pop B] ++ compile exp1 ++ [Pop A, assemblyOp A B]
--
-- compileBinNExp = compileBinExp compileNExp

compileNExp :: NExp -> State Memory [Mnemonic]
compileNExp (Vble x) = state $ \s -> ([], x:s)
compileNExp (NCte n) = doStackOp [Load A n]
-- compileNExp (Add exp1 exp2) = compileBinNExp exp1 exp2 ADD
-- compileNExp (Sub exp1 exp2) = compileBinNExp exp1 exp2 SUB
-- compileNExp (Div exp1 exp2) = compileBinNExp exp1 exp2 DIV
-- compileNExp (Mod exp1 exp2) = compileBinNExp exp1 exp2 MOD

-- compileBinBExp = compileBinExp compileBExp
--
compileBExp :: BExp -> State Memory [Mnemonic]
compileBExp (BCte b) = compileNExp (NCte (delta b))
-- compileBExp (Not exp) = doStackOp $ compileBExp exp ++ [Pop A, Load 1 B, ADDmod2 A B]
-- compileBExp (And exp1 exp2) = compileBinExp exp1 exp2
-- compileBExp (Or exp1 exp2) = compileBinExp exp1 exp2
-- compileBExp (Cmp op exp1 exp2) =
