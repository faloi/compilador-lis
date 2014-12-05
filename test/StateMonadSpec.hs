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

    it "acumula el estado tras cada bind" $ do
      let pop = state $ \(x:xs) -> (x, xs)
      let push x = state $ \xs -> ((), x:xs)
      runState (do push 8; push 9; x <- pop; push x) [] `shouldBe` ((), [9, 8])

    it "construida con updState, denota la mónada que resulta de actualizar el estado aplicando f" $ do
      runState (updState (+10)) 1 `shouldBe` ((), 11)

main :: IO ()
main = hspec spec
