module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

import qualified Data.Char as C
import Data.List (groupBy)

import Test.Hspec

data Point = Point Int Int deriving (Eq, Ord, Show)
data Range = Range (Int, Int) Int deriving (Eq, Ord, Show)

type Symbols = [(Char, Point)]
type Numbers = [(Int, Range)]

isSymbol :: Char -> Bool
isSymbol c = all ($ c) [not . C.isAlpha, not . C.isDigit, (/= '.')]

parsePoints :: String -> Symbols
parsePoints =
  let
    parseLine lno l = [(c, Point cno lno) | (cno, c) <- zip [1 ..] l, isSymbol c]
    parseLines ls = [line | (lno, l) <- zip [1 ..] ls, line <- parseLine lno l]
  in
    parseLines . lines

parseNumbers :: String -> Numbers
parseNumbers =
  let
    parseLine lno l =
      [ (read cs, Range (head is, last is) lno)
      | g@((_, c) : _) <- groupBy (\(_, a) (_, b) -> C.isDigit a && C.isDigit b) $ zip [1 ..] l
      , C.isDigit c
      , let (is, cs) = unzip g
      ]
    parseLines ls = [line | (lno, l) <- zip [1 ..] ls, line <- parseLine lno l]
  in
    parseLines . lines

isNeighbor :: Point -> Range -> Bool
isNeighbor (Point x y) (Range (a, b) c) = y `elem` [c - 1 .. c + 1] && x `elem` [a - 1 .. b + 1]

findNumbers :: Symbols -> Numbers -> Numbers
findNumbers ss ns = [n | (_, p) <- ss, n@(_, r) <- ns, isNeighbor p r]

findGear :: Symbols -> Numbers -> [Numbers]
findGear ss ns =
  [ gears
  | (c, p) <- ss
  , c == '*'
  , let gears = [n | n@(_, r) <- ns, isNeighbor p r]
  , length gears == 2
  ]

one :: String -> Int
one s =
  let ps = parsePoints s
      ns = parseNumbers s
  in  sum . map fst $ findNumbers ps ns

two :: String -> Int
two s =
  let ps = parsePoints s
      ns = parseNumbers s
  in  sum . map (product . map fst) $ findGear ps ns

main :: IO ()
main = do
  -- h <- openFile "txt/Day3/simple.txt" ReadMode
  -- h <- openFile "txt/Day3/simple2.txt" ReadMode
  h <- openFile "txt/Day3/input.txt" ReadMode
  t <- hGetContents h

  hspec
    $ describe "Day3"
    $ do
      it "part one" $ one t `shouldBe` 539590
      it "part two" $ two t `shouldBe` 80703636
