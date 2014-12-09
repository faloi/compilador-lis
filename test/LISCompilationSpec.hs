module LISCompilationSpec where

import Test.Hspec
import StateMonad
import AssemblyRepresentation
import LISRepresentation
import LISCompilation

shouldCompileTo compiler exp expected = evalState (compiler exp) [] `shouldBe` expected

shouldCompileNExpTo = shouldCompileTo compileNExp
shouldCompileBExpTo = shouldCompileTo compileBExp
shouldCompileCommTo = shouldCompileTo compileComm
shouldCompileBlockTo = shouldCompileTo compileBlock

spec :: Spec
spec = do
  describe "compileBlock" $ do
    it "puede compilar bloques" $ do
     [Assign "x" (NCte 10),
      Assign "y" (Add (NCte 1) (Vble "x"))] `shouldCompileBlockTo`
      [Load A 10, Push A,
      Pop A, Store A "x",
      Load A 1, Push A,
      Read A "x", Push A,
      Pop B, Pop A, ADD A B, Push A,
      Pop A, Store A "y"]

  describe "compileComm" $ do
    it "puede compilar un Skip" $ do
     Skip `shouldCompileCommTo` [NoOp]

    it "puede compilar un Assign" $ do
     Assign "x" (NCte 8) `shouldCompileCommTo`
      [Load A 8, Push A, Pop A, Store A "x"]

    it "puede compilar un If" $ do
     If (BCte True) [Assign "x" (NCte 8)] [Assign "x" (NCte 10)] `shouldCompileCommTo`
      [Load A 1, Push A,
      Pop A, JumpIfZ A "false_statements",
      Load A 8, Push A, Pop A, Store A "x",
      Jump "end_if",
      Mark "false_statements",
      Load A 10, Push A, Pop A, Store A "x",
      Mark "end_if"]

    it "puede compilar un While" $ do
     While (BCte True) [Assign "x" (NCte 4)] `shouldCompileCommTo`
      [Mark "begin_while",
        Load A 1, Push A,
        Pop A, JumpIfZ A "end_while",
        Load A 4, Push A, Pop A, Store A "x",
        Jump "begin_while",
      Mark "end_while"]

  describe "compileNExp" $ do
    it "puede compilar variables" $ do
     Vble "x" `shouldCompileNExpTo` [Read A "x", Push A]

    it "puede compilar constantes" $ do
     NCte 26 `shouldCompileNExpTo` [Load A 26, Push A]

    it "puede compilar sumas" $ do
     Add (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop B, Pop A, ADD A B, Push A]

    it "puede compilar restas" $ do
     Sub (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop B, Pop A, SUB A B, Push A]

    it "puede compilar divisiones" $ do
     Div (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop B, Pop A, DIV A B, Push A]

    it "puede compilar modulos" $ do
     Mod (NCte 26) (NCte 1) `shouldCompileNExpTo`
      [Load A 26, Push A,
      Load A 1, Push A,
      Pop B, Pop A, MOD A B, Push A]

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
      Pop B, Pop A, MUL A B, Push A]

    it "puede compilar un Or" $ do
     Or (BCte False) (BCte True) `shouldCompileBExpTo`
      [Load A 0, Push A,
      Load A 1, Push A,
      Pop B, Pop A, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un Equal" $ do
     Cmp Equal (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompEq A B, Push A]

    it "puede compilar un Greater" $ do
     Cmp Greater (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompGt A B, Push A]

    it "puede compilar un NotEqual" $ do
     Cmp NotEqual (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompEq A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un GreaterEqual" $ do
     Cmp GreaterEqual (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompGt A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompEq A B, Push A,
      Pop B, Pop A, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un Less" $ do
     Cmp Less (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompGt A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompEq A B, Push A,
      Pop B, Pop A, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

    it "puede compilar un LessEqual" $ do
     Cmp LessEqual (NCte 2) (NCte 3) `shouldCompileBExpTo`
      [Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompGt A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompEq A B, Push A,
      Pop B, Pop A, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A,
      Load A 2, Push A,
      Load A 3, Push A,
      Pop B, Pop A, CompEq A B, Push A,
      Pop B, Pop A, MUL A B, Push A,
      Pop A, Load B 1, ADDmod2 A B, Push A]

main :: IO ()
main = hspec spec

