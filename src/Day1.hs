module Main where

import Control.Monad (guard)
import Data.Functor

import System.IO (IOMode (ReadMode), hGetContents, openFile)

import Data.Char (digitToInt, isDigit)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)

import Test.Hspec
import Test.QuickCheck

combineDigits :: String -> Int
combineDigits =
  let keepDigits = (\s -> [head s, last s]) . filter isDigit
  in  read . keepDigits

go :: [Int] -> String -> [Int]
go acc [] = acc
go acc cs =
  let
    infixr 1 ?:
    (?:) = flip fromMaybe

    ws =
      zip
        [1 ..]
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

    parseSpelled =
      find ((`isPrefixOf` cs) . snd) ws <&> \(i, w) -> (i : acc, tail cs)

    parseNumber =
      guard (isDigit (head cs)) $> (digitToInt (head cs) : acc, tail cs)

    skip = (acc, tail cs)
  in
    uncurry go (parseSpelled ?: parseNumber ?: skip)

combineDigits' :: String -> Int
combineDigits' = (\xs -> head xs * 10 + last xs) . reverse . go []

main :: IO ()
main = do
  -- h <- openFile "txt/Day1/simple.txt" ReadMode
  -- h <- openFile "txt/Day1/simple2.txt" ReadMode
  h <- openFile "txt/Day1/input.txt" ReadMode
  t <- hGetContents h
  let ls = lines t

  let one = sum $ combineDigits <$> ls
  let two = sum $ combineDigits' <$> ls

  hspec
    $ describe "Day1"
    $ do
      it "part one" $ one `shouldBe` 54697
