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
     Vble "x" `shouldCompileNExpTo` []

    it "puede compilar constantes" $ do
     NCte 26 `shouldCompileNExpTo` [Load A 26, Push A]

  describe "compileBExp" $ do
    it "puede compilar constantes" $ do
     BCte False `shouldCompileBExpTo` [Load A 0, Push A]
     BCte True `shouldCompileBExpTo` [Load A 1, Push A]

    it "puede compilar un Not" $ do
     Not (BCte False) `shouldCompileBExpTo` [Load A 0, Push A, Pop A, Load B 1, ADDmod2 A B, Push A]

main :: IO ()
main = hspec spec

