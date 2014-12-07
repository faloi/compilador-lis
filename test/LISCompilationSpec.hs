module LISCompilationSpec where

import Test.Hspec
import StateMonad
import AssemblyRepresentation
import LISRepresentation
import LISCompilation

shouldCompileTo compiler exp expected = evalState (compiler exp) [] `shouldBe` expected

shouldCompileNExpTo = shouldCompileTo compileNExp
shouldCompileBExpTo = shouldCompileTo compileBExp

spec :: Spec
spec = do
  describe "compileNExp" $ do
    it "puede compilar variables" $ do
     Vble "x" `shouldCompileNExpTo` [Read A "x", Push A]

    it "puede compilar constantes" $ do
     NCte 26 `shouldCompileNExpTo` [Load A 26, Push A]

    it "puede compilar sumas" $ do
     Add (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop A, Pop B, ADD A B, Push A]

    it "puede compilar restas" $ do
     Sub (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop A, Pop B, SUB A B, Push A]

    it "puede compilar divisiones" $ do
     Div (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop A, Pop B, DIV A B, Push A]

    it "puede compilar modulos" $ do
     Mod (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop A, Pop B, MOD A B, Push A]

  describe "compileBExp" $ do
    it "puede compilar constantes" $ do
     BCte False `shouldCompileBExpTo` [Load A 0, Push A]
     BCte True `shouldCompileBExpTo` [Load A 1, Push A]

    it "puede compilar un Not" $ do
     Not (BCte False) `shouldCompileBExpTo`
      [Load A 0, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un And" $ do
     And (BCte False) (BCte True) `shouldCompileBExpTo`
      [Load A 0, Push A,
      Load A 1, Push A,
      Pop A, Pop B, MUL A B, Push A]

    it "puede compilar un Or" $ do
     Or (BCte False) (BCte True) `shouldCompileBExpTo`
      [Load A 0, Push A,
      Load A 1, Push A,
      Pop A, Pop B, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un Equal" $ do
     Cmp Equal (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompEq A B, Push A]

    it "puede compilar un Greater" $ do
     Cmp Greater (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompGt A B, Push A]

    it "puede compilar un NotEqual" $ do
     Cmp NotEqual (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompEq A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un GreaterEqual" $ do
     Cmp GreaterEqual (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompGt A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompEq A B, Push A,
      Pop A, Pop B, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un Less" $ do
     Cmp Less (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompGt A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompEq A B, Push A,
      Pop A, Pop B, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un LessEqual" $ do
     Cmp LessEqual (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompGt A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompEq A B, Push A,
      Pop A, Pop B, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop A, Pop B, CompEq A B, Push A,
      Pop A, Pop B, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

main :: IO ()
main = hspec spec

