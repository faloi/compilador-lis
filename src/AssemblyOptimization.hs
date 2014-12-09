module AssemblyOptimization (
    optimizeProgram
) where

import AssemblyRepresentation

-- Ejercicio 5 (Opcional): Proponer e implementar posibles optimizaciones.


-- optimizeAssProg

optimizeProgram :: AssemblyProgram -> AssemblyProgram
optimizeProgram (AssemblyProgram mns) = AssemblyProgram (optimizeMnemonics mns)


-- optimizeMnemonics

optimizeMnemonics :: [Mnemonic] -> [Mnemonic]
optimizeMnemonics (Push x : Pop y : ms)
  | x == y = optimizeMnemonics ms

optimizeMnemonics (Store r1 v1 : Read r2 v2 : ms)
  | r1 == r2 && v1 == v2 = optimizeMnemonics $ (Store r1 v1):ms

optimizeMnemonics (m:ms) = m : optimizeMnemonics ms
optimizeMnemonics [] = []
