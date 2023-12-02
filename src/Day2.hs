module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

import Data.Function
import Data.Functor

import Data.Char (isDigit)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Test.Hspec

-----------
-- Types --
-----------
data Color = Blue Int | Red Int | Green Int deriving (Show)
type Set = [Color]
data Game = Game Int [Set] deriving (Show)

------------
-- Parser --
------------
type Parser = Parsec Void String

sc :: Parser ()
sc = skipMany space1

lexeme :: String -> Parser String
lexeme s = string s <* sc

pDigit :: Parser Int
pDigit = read <$> many (satisfy isDigit) <* sc

pColor :: Parser Color
pColor = do
  i <- pDigit
  color <- choice [Blue <$ lexeme "blue", Red <$ lexeme "red", Green <$ lexeme "green"]
  return $ color i

pSet :: Parser Set
pSet = pColor `sepBy1` lexeme ","

pGame :: Parser Game
pGame = do
  i <- lexeme "Game" *> pDigit <* lexeme ":"
  Game i <$> pSet `sepBy1` lexeme ";"

pAll :: Parser [Game]
pAll = (pGame `sepEndBy1` many eol) <* eof

go :: String -> [Game]
go s = case runParser pAll "" s of
  Left _ -> undefined
  Right r -> r

--------------
-- Solution --
--------------
type RGB = [Int]

toRgb :: RGB -> Set -> RGB
toRgb acc [] = acc
toRgb [r, g, b] (x : xs) =
  toRgb
    ( case x of
        Red i -> [r + i, g, b]
        Green i -> [r, g + i, b]
        Blue i -> [r, g, b + i]
    )
    xs

mergeRgb :: RGB -> RGB -> RGB
mergeRgb = zipWith max

maxSet :: [Set] -> RGB
maxSet = foldl1 mergeRgb . map (toRgb [0, 0, 0])

--------------
-- Part one --
--------------
filterGame :: (RGB -> Bool) -> [Game] -> [Game]
filterGame thres = filter (\(Game _ s) -> thres (maxSet s))

--------------
-- Part two --
--------------
powerGame :: [Game] -> [Int]
powerGame = map (\(Game _ s) -> product (maxSet s))

main :: IO ()
main = do
  -- h <- openFile "txt/Day2/simple.txt" ReadMode
  -- h <- openFile "txt/Day2/simple2.txt" ReadMode
  h <- openFile "txt/Day2/input.txt" ReadMode
  t <- hGetContents h

  let parsed = go t

  let one =
        sum
          $ (\(Game i _) -> i)
          <$> filterGame (all (uncurry (>=)) . zip [12, 13, 14]) parsed

  let two = sum . powerGame $ parsed

  hspec
    $ describe "Day2"
    $ do
      it "part one" $ one `shouldBe` 3035
      it "part two" $ two `shouldBe` 66027
