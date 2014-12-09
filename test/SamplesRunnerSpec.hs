module SamplesRunnerSpec where

import Test.Hspec
import Data.Maybe
import Data.Map as Map
import ParserLib
import LISParser
import LISCompilation
import AssemblyOptimization
import AssemblyExecution

run filename = do code <- readFile filename
                  let prog = parseLIS $ code
                      res = execProgram $
                            optimizeProgram $
                            compileProgram $
                            prog
                   in do return res

parseLIS = fst . (bestFirst lisParser) . lisLexer

running filename = run $ "samples/" ++ filename ++ ".lis"
shouldHaveResult mvs expectedResult = do result <- mvs; (fromJust.(Map.lookup "return").datamem) result `shouldBe` expectedResult

spec :: Spec
spec = do
  describe "Running sample" $ do
    it "factorial" $ do
      running "factorial" `shouldHaveResult` 720

    it "nToBinary" $ do
      running "nToBinary" `shouldHaveResult` 11101011

    it "boolTest" $ do
      running "boolTest" `shouldHaveResult` 1

    it "summatory" $ do
      running "summatory" `shouldHaveResult` 55

    it "fibonacci" $ do
      running "fibonacci" `shouldHaveResult` 6765

main :: IO ()
main = hspec spec

