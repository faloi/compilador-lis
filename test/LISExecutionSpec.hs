module LISExecutionSpec where

import Test.Hspec
import AssemblyExecution
import AssemblyRepresentation
import LISRepresentation
import LISCompilation
import Data.Maybe
import Data.Map as Map

afterRunning = execProgram.compileProgram.Program
memoryShouldBe mvs expectedVariables = datamem mvs `shouldBe` fromList expectedVariables
shouldEqual nexp expected = afterRunning [Assign "x" nexp] `memoryShouldBe` [("x", expected)]
shouldEqualBool bexp expected = afterRunning [If bexp [Assign "x" (NCte 1)] [Assign "x" (NCte 0)]] `memoryShouldBe` [("x", delta expected)]

spec :: Spec
spec = do
  describe "LIS puede ejecutar correctamente" $ do
    describe "operaciones numericas:" $ do
      it "una suma" $ do
        Add (NCte 8) (NCte 7) `shouldEqual` 15

      it "una resta" $ do
        Sub (NCte 8) (NCte 7) `shouldEqual` 1

      it "una division" $ do
        Div (NCte 4) (NCte 2) `shouldEqual` 2

      it "una multiplicacion" $ do
        Mul (NCte 3) (NCte 5) `shouldEqual` 15

      it "un modulo" $ do
        Mod (NCte 8) (NCte 2) `shouldEqual` 0

    describe "operaciones booleanas:" $ do
      describe "constantes" $ do
        it "un True" $ do
          BCte True `shouldEqualBool` True
        it "un False" $ do
          BCte False `shouldEqualBool` False

main :: IO ()
main = hspec spec

