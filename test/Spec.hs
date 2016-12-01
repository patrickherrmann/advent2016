import Protolude
import Test.Hspec
import Day01

main :: IO ()
main = hspec $ do
  context "Day 1" $ do
    input <- runIO $ readFile "inputs/Day01.txt"
    specify "Part 1" $ do
      let is = parseInstructions input
      let (Position _ l) = followInstructions is
      manhattanDistance l `shouldBe` 250