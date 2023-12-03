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
    parseLine lno = map (\(cno, c) -> (c, Point cno lno)) . filter (isSymbol . snd) . zip [1 ..]
  in
    concatMap (uncurry parseLine) . zip [1 ..] . lines

parseNumbers :: String -> Numbers
parseNumbers =
  let
    parseLine lno =
      map (\l -> let (is, cs) = unzip l in (read cs, Range (head is, last is) lno))
        . filter (\((_, c) : _) -> C.isDigit c)
        . groupBy (\(_, a) (_, b) -> C.isDigit a && C.isDigit b)
        . zip [1 ..]
  in
    concatMap (uncurry parseLine) . zip [1 ..] . lines

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
