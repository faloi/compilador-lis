module LISCompilation (
    compileProgram
) where

import AssemblyRepresentation
import LISRepresentation
import StateMonad

-- Ejercicio 4: Completar el modulo LISCompilation.

type Memory = -- COMPLETAR

-- compileProgram

compileProgram :: Program -> AssemblyProgram
compileProgram (Program bl) = AssemblyProgram (evalState (compileBlock bl) startState)


-- initialIx

startState :: Memory
startState = -- COMPLETAR


-- compileBlock

compileBlock :: Block -> State Memory [Mnemonic]
-- COMPLETAR


-- compileComm

compileComm :: Command -> State Memory [Mnemonic]
-- COMPLETAR


-- compileNExp

compileNExp :: NExp -> State Memory [Mnemonic]
-- COMPLETAR


-- compileNExp

compileBExp :: BExp -> State Memory [Mnemonic]
-- COMPLETAR

