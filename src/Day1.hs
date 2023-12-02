{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Char (digitToInt, isDigit)

import Data.List (isPrefixOf)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

combineDigits :: String -> Int
combineDigits =
  let keepDigits = (\s -> [head s, last s]) . filter isDigit
  in  read . keepDigits

go :: [Int] -> String -> [Int]
go acc [] = acc
go acc cs =
  go
    if
      | "one" `isPrefixOf` cs -> 1 : acc
      | "two" `isPrefixOf` cs -> 2 : acc
      | "three" `isPrefixOf` cs -> 3 : acc
      | "four" `isPrefixOf` cs -> 4 : acc
      | "five" `isPrefixOf` cs -> 5 : acc
      | "six" `isPrefixOf` cs -> 6 : acc
      | "seven" `isPrefixOf` cs -> 7 : acc
      | "eight" `isPrefixOf` cs -> 8 : acc
      | "nine" `isPrefixOf` cs -> 9 : acc
      | isDigit (head cs) -> digitToInt (head cs) : acc
      | otherwise -> acc
    (tail cs)

combineDigits' :: String -> Int
combineDigits' = (\xs -> head xs * 10 + last xs) . reverse . go []

main :: IO ()
main = do
  -- h <- openFile "txt/day1/simple.txt" ReadMode
  -- h <- openFile "txt/day1/simple2.txt" ReadMode
  h <- openFile "txt/day1/input.txt" ReadMode
  t <- hGetContents h
  let ls = lines t

  print "part one"
  print $ sum $ combineDigits <$> ls

  print "part two"
  print $ sum $ combineDigits' <$> ls
