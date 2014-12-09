module AssemblyOptimizationSpec where

import Test.Hspec
import AssemblyExecution
import AssemblyRepresentation
import AssemblyOptimization
import LISRepresentation
import LISCompilation
import Data.Maybe
import Data.Map as Map

optimize = optimizeProgram.AssemblyProgram
shouldBeOptimizedTo program optimizedProgram = optimize program `shouldBe` AssemblyProgram optimizedProgram

spec :: Spec
spec = do
  describe "Optimizar la secuencia" $ do
    describe "[Push x, Pop y]" $ do
      it "cuando x == y, lo cambia por []" $ do
        [Push A, Pop A] `shouldBeOptimizedTo` []
      it "cuando x /= y, no hace nada" $ do
        [Push A, Pop B] `shouldBeOptimizedTo` [Push A, Pop B]

main :: IO ()
main = hspec spec

