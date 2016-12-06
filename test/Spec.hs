import Protolude
import Test.Hspec
import Day01
import Day02
import Day03
import Day04
import Day05

main :: IO ()
main = hspec $ do

  context "Day 1" $ do
    input <- runIO $ readFile "inputs/Day01.txt"
    let is = parseDay01 input
    let ps = followInstructions is
    specify "Part 1" $ do
      let Just (Position _ l) = lastMay ps
      manhattanDistance l `shouldBe` 250
    specify "Part 2" $ do
      let ls = map (\(Position _ l) -> l) ps
      manhattanDistance (firstDup ls) `shouldBe` 151

  context "Day 2" $ do
    input <- runIO $ readFile "inputs/Day02.txt"
    let lines = parseDay02 input
    specify "Part 1" $ do
      let code = findCode moveDirection1 lines
      code `shouldBe` "82958"
    specify "Part 2" $ do
      let code = findCode moveDirection2 lines
      code `shouldBe` "B3DB8"

  context "Day 3" $ do
    input <- runIO $ readFile "inputs/Day03.txt"
    let ts = parseDay03 input
    specify "Part 1" $ do
      triangleCount ts `shouldBe` 917
    specify "Part 2" $ do
      let ts' = transformInput ts
      triangleCount ts' `shouldBe` 1649

  context "Day 4" $ do
    input <- runIO $ readFile "inputs/Day04.txt"
    let rs = parseDay04 input
    specify "Part 1" $ do
      sectorIdSum rs `shouldBe` 409147
    specify "Part 2" $ do
      let rs' = map decryptRoom rs
      let Just room = find (\r -> "northpole" `elem` roomName r) rs'
      sectorId room `shouldBe` 991

  context "Day 5" $ do
    input <- runIO $ readFile "inputs/Day05.txt"
    let code = parseDay05 input
    specify "Part 1" $ do
      let ihs = take 8 $ interestingHashes code
      let pw = map (getPasswordChar . snd) ihs
      pw `shouldBe` "f77a0e6e"