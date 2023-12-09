module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

import Test.Hspec

import qualified Data.Set as S

import Control.Monad
import Data.Char
import Data.Void
import Debug.Trace
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Card = Card Int (S.Set Int) [Int] deriving (Show)

sc :: Parser ()
sc = skipMany hspace1

lexeme :: Parser a -> Parser a
lexeme p = p <* sc

symbol :: String -> Parser String
symbol s = string s <* sc

pDigit :: Parser Int
pDigit = read <$> takeWhile1P (Just "digit") isDigit

pCard :: Parser Card
pCard = do
  nb <- between (symbol "Card") (symbol ":") pDigit
  ws <- S.fromList <$> pDigit `sepEndBy1` hspace
  void $ symbol "|"
  ns <- pDigit `sepEndBy1` hspace
  return $ Card nb ws ns

pCards :: Parser [Card]
pCards = (pCard `sepEndBy1` eol) <* eof

go :: String -> [Card]
go input =
  let (Right res) = runParser pCards "file" input
  in  res

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

one :: [Card] -> Int
one =
  let
    solveLine (Card _ ws ns) =
      (\n -> if n == 0 then 0 else 2 ^ (n - 1)) . count id . map (`S.member` ws) $ ns
  in
    sum . map solveLine

two :: [Card] -> Int
two =
  let
    solveLine (Card _ ws ns) = count id . map (`S.member` ws) $ ns
    copies = repeat 1

    inc _ xs 0 = xs
    inc _ [] _ = []
    inc k (x : xs) acc = (x + k) : inc k xs (acc - 1)

    aux [] _ = []
    aux (c : cs) (k : ks) =
      let
        n = solveLine c
        ks' = inc k ks n
      in
        k : aux cs ks'
  in
    sum . flip aux copies

main :: IO ()
main = do
  -- h <- openFile "txt/Day4/simple.txt" ReadMode
  h <- openFile "txt/Day4/input.txt" ReadMode
  t <- hGetContents h

  hspec
    $ describe "Day4"
    $ do
      it "part one" $ (one . go) t `shouldBe` 20829
      it "part one" $ (two . go) t `shouldBe` 12648035
