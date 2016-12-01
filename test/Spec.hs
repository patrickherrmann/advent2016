import Protolude
import Test.Hspec
import Day01

main :: IO ()
main = hspec $ do
  context "Day 1" $ do
    input <- runIO $ readFile "inputs/Day01.txt"
    specify "Part 1" $ do
      reverseCaps input `shouldBe` "ELPMAS"
    specify "Part 2" $ do
      addBang input `shouldBe` "sample!"