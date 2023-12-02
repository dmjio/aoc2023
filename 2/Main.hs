{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = do
  ls <- lines <$> readFile "2.txt"
  let games = zip [1..] (mkRounds <$> ls)
  print $ sum (possibleGames m games)
  print $ findPower games

type Game  = (Int, [Round])
type Round = (Int, Int, Int) -- (R,B,G)

m = (12,14,13)

-- part 2

findPower :: [Game] -> Int
findPower games = powerSet (fewestRequired <$> fmap snd games)

fewestRequired :: [Round] -> Round
fewestRequired rounds = foldl' go (0,0,0) rounds
  where
    go (r,b,g) (r',b',g') = (max r r', max b b', max g g')

powerSet :: [Round] -> Int
powerSet = sum . fmap powerCube
  where
    powerCube :: Round -> Int
    powerCube (r,b,g) = r * b * g

-- part 1

possibleGames :: Round -> [Game] -> [Int]
possibleGames maxRound games = fst <$> filter possible games
  where
    possible (_, rounds) = not $ any (greaterThanMax maxRound) rounds

greaterThanMax :: Round -> Round -> Bool
greaterThanMax (mr,mb,mg) (r,b,g) = r > mr || b > mb || g > mg

dropPrefix :: String -> String
dropPrefix = drop 2 . dropWhile (/=':')

mkRound :: String -> Round
mkRound xs = foldr f (0,0,0) (strip <$> splitOn "," xs)
  where
    f (parse -> (x, "red"))   (r,b,g) = (x,b,g)
    f (parse -> (x, "blue"))  (r,b,g) = (r,x,g)
    f (parse -> (x, "green")) (r,b,g) = (r,b,x)

parse :: String -> (Int, String)
parse s = (read (takeWhile isDigit s), dropWhile (not . isAlpha) s)

chunkRounds :: String -> [String]
chunkRounds = splitOn ";"

strip :: String -> String
strip = reverse . dropWhile (==' ') . reverse . dropWhile (== ' ')

mkRounds :: String -> [Round]
mkRounds raw = do
  round <- chunkRounds (dropPrefix raw)
  pure (mkRound round)
