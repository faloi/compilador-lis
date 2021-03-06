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
    describe "un if" $ do
      it "cuando la condicion es verdadera" $ do
        afterRunning [
          If (BCte True)
            [Assign "x" (NCte 28)]
            [Assign "x" (NCte 12)]] `memoryShouldBe` [("x", 28)]

      it "cuando la condicion es falsa" $ do
        afterRunning [
          If (BCte False)
            [Assign "x" (NCte 28)]
            [Assign "x" (NCte 12)]] `memoryShouldBe` [("x", 12)]

      it "cuando hay mas de uno" $ do
        afterRunning [
          If (BCte False)
            [Assign "x" (NCte 28)]
            [Assign "x" (NCte 12)],

          If (BCte False)
            [Assign "y" (NCte 28)]
            [Assign "y" (NCte 12)]] `memoryShouldBe` [("x", 12), ("y", 12)]

    describe "un while" $ do
      it "cuando la condicion es falsa desde el principio" $ do
        afterRunning [
          Assign "x" (NCte 8),
          While (BCte False) [
            Assign "x" (NCte 25)]] `memoryShouldBe` [("x", 8)]

      it "mientras la condicion es verdadera" $ do
        afterRunning [
          Assign "x" (NCte 0),
          While (Cmp Less (Vble "x") (NCte 10)) [
            Assign "x" (Add (Vble "x") (NCte 1))]] `memoryShouldBe` [("x", 10)]

      it "cuando hay mas de uno" $ do
        afterRunning [
          Assign "x" (NCte 0),
          While (Cmp Less (Vble "x") (NCte 10)) [
            Assign "x" (Add (Vble "x") (NCte 1))],

          Assign "y" (NCte 0),
          While (Cmp Less (Vble "y") (NCte 10)) [
            Assign "y" (Add (Vble "y") (NCte 1))]] `memoryShouldBe` [("x", 10), ("y", 10)]

    describe "expresiones numericas:" $ do
      it "una variable" $ do
        afterRunning [
          Assign "x" (NCte 8),
          Assign "y" (Vble "x")] `memoryShouldBe` [("x", 8), ("y", 8)]

      it "una constante" $ do
        NCte 8 `shouldEqual` 8

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

    describe "expresiones booleanas:" $ do
      describe "constantes" $ do
        it "un True" $ do
          BCte True `shouldEqualBool` True
        it "un False" $ do
          BCte False `shouldEqualBool` False

      describe "un Not" $ do
        it "True" $ do
          Not (BCte True) `shouldEqualBool` False
        it "False" $ do
          Not (BCte False) `shouldEqualBool` True

      describe "un And" $ do
        it "True False" $ do
          And (BCte True) (BCte False) `shouldEqualBool` False
        it "False True" $ do
          And (BCte False) (BCte True) `shouldEqualBool` False
        it "False False" $ do
          And (BCte False) (BCte False) `shouldEqualBool` False
        it "True True" $ do
          And (BCte True) (BCte True) `shouldEqualBool` True

      describe "un Or" $ do
        it "True False" $ do
          Or (BCte True) (BCte False) `shouldEqualBool` True
        it "False True" $ do
          Or (BCte False) (BCte True) `shouldEqualBool` True
        it "False False" $ do
          Or (BCte False) (BCte False) `shouldEqualBool` False
        it "True True" $ do
          Or (BCte True) (BCte True) `shouldEqualBool` True

      describe "un Equal" $ do
        it "cuando es verdadero" $ do
          Cmp Equal (NCte 3) (NCte 3) `shouldEqualBool` True
        it "cuando es falso" $ do
          Cmp Equal (NCte 3) (NCte 2) `shouldEqualBool` False

      describe "un NotEqual" $ do
        it "cuando es verdadero" $ do
          Cmp NotEqual (NCte 3) (NCte 3) `shouldEqualBool` False
        it "cuando es falso" $ do
          Cmp NotEqual (NCte 3) (NCte 2) `shouldEqualBool` True

      describe "un Greater" $ do
        it "cuando es mayor" $ do
          Cmp Greater (NCte 3) (NCte 2) `shouldEqualBool` True
        it "cuando es igual" $ do
          Cmp Greater (NCte 2) (NCte 2) `shouldEqualBool` False
        it "cuando es menor" $ do
          Cmp Greater (NCte 1) (NCte 2) `shouldEqualBool` False

      describe "un GreaterEqual" $ do
        it "cuando es mayor" $ do
          Cmp GreaterEqual (NCte 3) (NCte 2) `shouldEqualBool` True
        it "cuando es igual" $ do
          Cmp GreaterEqual (NCte 2) (NCte 2) `shouldEqualBool` True
        it "cuando es menor" $ do
          Cmp GreaterEqual (NCte 1) (NCte 2) `shouldEqualBool` False

      describe "un Less" $ do
        it "cuando es mayor" $ do
          Cmp Less (NCte 3) (NCte 2) `shouldEqualBool` False
        it "cuando es igual" $ do
          Cmp Less (NCte 2) (NCte 2) `shouldEqualBool` False
        it "cuando es menor" $ do
          Cmp Less (NCte 1) (NCte 2) `shouldEqualBool` True

      describe "un LessEqual" $ do
        it "cuando es mayor" $ do
          Cmp LessEqual (NCte 3) (NCte 2) `shouldEqualBool` False
        it "cuando es igual" $ do
          Cmp LessEqual (NCte 2) (NCte 2) `shouldEqualBool` True
        it "cuando es menor" $ do
          Cmp LessEqual (NCte 1) (NCte 2) `shouldEqualBool` True

main :: IO ()
main = hspec spec

