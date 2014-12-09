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
  describe "Al optimizar un AssemblyProgram" $ do
    describe "si hay un Push seguido de un Pop" $ do
      it "sobre el mismo registro, se eliminan" $ do
        [Push A, Pop A] `shouldBeOptimizedTo` []
      it "sobre registros diferentes, no pasa nada" $ do
        [Push A, Pop B] `shouldBeOptimizedTo` [Push A, Pop B]

    describe "si hay un Store seguido de un Read" $ do
      it "sobre el mismo registro y variable, se elimina el Read" $ do
        [Store A "n", Read A "n", Read A "n", Read A "n"] `shouldBeOptimizedTo` [Store A "n"]
      it "sobre distinto registro o variable, no pasa nada" $ do
        [Store A "x", Read A "y"] `shouldBeOptimizedTo` [Store A "x", Read A "y"]

main :: IO ()
main = hspec spec

