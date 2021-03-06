import Protolude
import Test.Hspec
import qualified Data.Text as Text
import qualified Data.MultiMap as MMap
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10

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
    let ihs = interestingHashes code
    specify "Part 1" $ do
      let pw = take 8 $ map getPasswordChar ihs
      pw `shouldBe` "f77a0e6e"
    specify "Part 2" $ do
      let ps = map getIndexAndChar ihs
      let pw = constructPassword ps
      pw `shouldBe` "999828ec"

  context "Day 6" $ do
    input <- runIO $ readFile "inputs/Day06.txt"
    let cols = parseDay06 input
    specify "Part 1" $ do
      let message = decodeMessage1 cols
      message `shouldBe` "afwlyyyq"
    specify "Part 2" $ do
      let message = decodeMessage2 cols
      message `shouldBe` "bhkzekao"

  context "Day 7" $ do
    input <- runIO $ readFile "inputs/Day07.txt"
    let ips = parseDay07 input
    specify "Part 1" $ do
      let tlsIps = filter supportsTls ips
      length tlsIps `shouldBe` 110
    specify "Part 2" $ do
      let sslIps = filter supportsSsl ips
      length sslIps `shouldBe` 242

  context "Day 8" $ do
    input <- runIO $ readFile "inputs/Day08.txt"
    let is = parseDay08 input
    let g = runInstructions is
    specify "Part 1" $ do
      measureVoltage g `shouldBe` 106
    specify "Part 2" $ do
      let expectedGrid = 
            ".##.. ####. #.... ####. #.... .##.. #...# ####. .##.. .###.\n\
            \#..#. #.... #.... #.... #.... #..#. #...# #.... #..#. #....\n\
            \#.... ###.. #.... ###.. #.... #..#. .#.#. ###.. #.... #....\n\
            \#.... #.... #.... #.... #.... #..#. ..#.. #.... #.... .##..\n\
            \#..#. #.... #.... #.... #.... #..#. ..#.. #.... #..#. ...#.\n\
            \.##.. #.... ####. ####. ####. .##.. ..#.. #.... .##.. ###..\n"
      displayGrid g `shouldBe` expectedGrid

  context "Day 9" $ do
    input <- runIO $ readFile "inputs/Day09.txt"
    specify "Part 1" $ do
      Text.length (decompress1 input) `shouldBe` 74532
    specify "Part 2" $ do
      decompress2 input `shouldBe` 11558231665

  context "Day 10" $ do
    input <- runIO $ readFile "inputs/Day10.txt"
    let is = parseDay10 input
    let s = runBotSimulation is
    specify "Part 1" $ do
      let h = _history s
      let correctComparison (botId, lo, hi) = lo == 17 && hi == 61
      let Just (botId, _, _) = find correctComparison h
      botId `shouldBe` 157
    specify "Part 2" $ do
      let bs = _bins s
      let vals = [0, 1, 2] >>= flip MMap.lookup bs
      product vals `shouldBe` 1085