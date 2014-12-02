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
optimizeMnemonics = id -- REEMPLAZAR

