module StateMonadSpec where

import Test.Hspec
import StateMonad

spec :: Spec
spec = do
  describe "Una monada State" $ do
    it "puede construirse con return" $ do
      runState (return 8) [1] `shouldBe` (8, [1])

    it "al evaluarse y retorna su valor" $ do
      evalState (return 8) [1] `shouldBe` 8

    it "al ejecutarse y retorna su estado" $ do
      execState (return 8) [1] `shouldBe` [1]

    it "construida con getState, denota la mónada que retorna su estado actual" $ do
      runState getState [1] `shouldBe` ([1], [1])

main :: IO ()
main = hspec spec
