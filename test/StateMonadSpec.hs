module StateMonadSpec where

import Test.Hspec
import StateMonad

spec :: Spec
spec = do
  describe "La monada State" $ do
    it "sabe hacer return" $ do
      runState (return 8) [1] `shouldBe` (8, [1])

    it "puede evaluarse, dandole un estado" $ do
      evalState (return 8) [1] `shouldBe` 8

main :: IO ()
main = hspec spec
