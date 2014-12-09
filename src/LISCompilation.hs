module LISCompilation (
    compileProgram,
    compileBlock,
    compileComm,
    compileNExp,
    compileBExp
) where

import AssemblyRepresentation
import LISRepresentation
import StateMonad
import Control.Monad

-- La memoria es una tupla que guarda la cantidad de ifs y de whiles que hay en el codigo
type Memory = (Int, Int)

compileProgram :: Program -> AssemblyProgram
compileProgram (Program bl) = AssemblyProgram (evalState (compileBlock bl) startState)

startState :: Memory
startState = (0, 0)

(<++>) :: State Memory [Mnemonic] -> State Memory [Mnemonic] -> State Memory [Mnemonic]
(<++>) = liftM2 (++)

compileBlock :: Block -> State Memory [Mnemonic]
compileBlock = foldl (<++>) (return []).map compileComm

generateIfLabels :: State Memory (String, String)
generateIfLabels =
  do  (cantidadIfs, _) <- getState
      let end_if = "end_if #" ++ show cantidadIfs
      let false_branch = "false_branch #" ++ show cantidadIfs
      updState $ \(ifs, whiles) -> (ifs + 1, whiles)
      return (end_if, false_branch)

generateWhileLabels :: State Memory (String, String)
generateWhileLabels =
  do  (_, cantidadWhiles) <- getState
      let begin_while = "begin_while #" ++ show cantidadWhiles
      let end_while = "end_while #" ++ show cantidadWhiles
      updState $ \(ifs, whiles) -> (ifs, whiles + 1)
      return (begin_while, end_while)

compileComm :: Command -> State Memory [Mnemonic]

compileComm Skip = return [NoOp]

compileComm (Assign var nexp) =
  do  val <- compileNExp nexp
      return $ val ++ [Pop A, Store A var]

compileComm (If bexp trueBlock falseBlock) =
  do  trueOps <- compileBlock trueBlock
      falseOps <- compileBlock falseBlock
      condition <- compileBExp bexp
      (end_if, false_branch) <- generateIfLabels
      return $ condition ++ [Pop A, JumpIfZ A false_branch] ++ trueOps ++ [Jump end_if, Mark false_branch] ++ falseOps ++ [Mark end_if]

compileComm (While bexp block) =
  do  condition <- compileBExp bexp
      body <- compileBlock block
      (begin_while, end_while) <- generateWhileLabels
      return $ [Mark begin_while] ++ condition ++ [Pop A, JumpIfZ A end_while] ++ body ++ [Jump begin_while, Mark end_while]

doStackOp :: [Mnemonic] -> State Memory [Mnemonic]
doStackOp mnemonics = return (mnemonics ++ [Push A])

compileBinExp compile exp1 exp2 assemblyOp =
  do  ops1 <- compile exp1
      ops2 <- compile exp2
      doStackOp $ ops1 ++ ops2 ++ [Pop B, Pop A, assemblyOp A B]

compileBinNExp = compileBinExp compileNExp

compileNExp :: NExp -> State Memory [Mnemonic]
compileNExp (Vble x) = doStackOp [Read A x]
compileNExp (NCte n) = doStackOp [Load A n]
compileNExp (Add exp1 exp2) = compileBinNExp exp1 exp2 ADD
compileNExp (Sub exp1 exp2) = compileBinNExp exp1 exp2 SUB
compileNExp (Div exp1 exp2) = compileBinNExp exp1 exp2 DIV
compileNExp (Mul exp1 exp2) = compileBinNExp exp1 exp2 MUL
compileNExp (Mod exp1 exp2) = compileBinNExp exp1 exp2 MOD

compileBinBExp = compileBinExp compileBExp

compileBExp :: BExp -> State Memory [Mnemonic]
compileBExp (BCte b) = compileNExp (NCte (delta b))
compileBExp (Not exp) =
  do  ops <- compileBExp exp
      doStackOp $ ops ++ [Pop A, Load B 1, ADDmod2 A B]

compileBExp (And exp1 exp2) = compileBinBExp exp1 exp2 MUL
compileBExp (Or exp1 exp2) = compileBExp $ Not (And (Not (And exp1 exp1)) (Not (And exp2 exp2)))
compileBExp (Cmp Equal exp1 exp2) = compileBinNExp exp1 exp2 CompEq
compileBExp (Cmp Greater exp1 exp2) = compileBinNExp exp1 exp2 CompGt
compileBExp (Cmp NotEqual exp1 exp2) = compileBExp $ Not (Cmp Equal exp1 exp2)
compileBExp (Cmp GreaterEqual exp1 exp2) = compileBExp $ Or (Cmp Greater exp1 exp2) (Cmp Equal exp1 exp2)
compileBExp (Cmp Less exp1 exp2) = compileBExp $ Not (Cmp GreaterEqual exp1 exp2)
compileBExp (Cmp LessEqual exp1 exp2) = compileBExp $ Or (Cmp Less exp1 exp2) (Cmp Equal exp1 exp2)
