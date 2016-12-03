import Protolude
import Test.Hspec
import Day01

main :: IO ()
main = hspec $ do
  context "Day 1" $ do
    input <- runIO $ readFile "inputs/Day01.txt"
    let is = parseInstructions input
    let ps = followInstructions is
    specify "Part 1" $ do
      let Just (Position _ l) = lastMay ps
      manhattanDistance l `shouldBe` 250
    specify "Part 2" $ do
      let ls = map (\(Position _ l) -> l) ps
      manhattanDistance (firstDup ls) `shouldBe` 151