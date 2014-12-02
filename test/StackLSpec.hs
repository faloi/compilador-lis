module StackLSpec where

import Test.Hspec
import StackL

spec :: Spec
spec = do
  describe "Un stack" $ do
    it "vacio esta empty" $ do
      isEmpty empty `shouldBe` True

    it "no vacio no esta empty" $ do
      isEmpty (push 3 empty) `shouldBe` False

    it "se comporta de manera LIFO" $ do
      (top $ push 3 $ push 4 $ push 5 empty) `shouldBe` 3

    it "devuelve un nuevo stack al hacer pop" $ do
      (top $ pop $ push 4 $ push 5 empty) `shouldBe` 5

main :: IO ()
main = hspec spec
