module Main where

import Data.List

main :: IO ()
main = do
  ls <- fmap readLine . lines <$> readFile "9.txt"
  print (part1 ls)
  print (part2 ls)

readLine :: String -> [Int]
readLine = map read . words

diff :: [Int] -> [Int]
diff xs = zipWith subtract xs (tail xs)

part1 xs = sum (solve <$> xs)
part2 = part1 . fmap reverse

solve :: [Int] -> Int
solve
  = last
  . head
  . predict
  . takeWhile ((/=0) . sum)
  . iterate diff

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

predict :: [[Int]] -> [[Int]]
predict xs = reverse $ go (reverse xs)
  where
    go (x:y:xs) = x : go (snoc (last x + last y) y : xs)
    go xs = xs

input =
  "0 3 6 9 12 15\n\
  \1 3 6 10 15 21\n\
  \10 13 16 21 30 45"
