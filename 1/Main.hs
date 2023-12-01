module Main where

import           Data.Char  ( digitToInt, isDigit )
import           Data.List  ( find, isPrefixOf )
import           Data.Map   ( Map )
import qualified Data.Map   as M ( elems, fromList, keys, lookup )
import           Data.Maybe ( mapMaybe )

main :: IO ()
main = do
  xs <- lines <$> readFile "1.txt"
  print $ sum [ read [ takeFirstDigit x, takeLastDigit x ] | x <- xs ]
  print $ sum (mapMaybe createNum xs)

-- part 1

takeFirstDigit :: String -> Char
takeFirstDigit = head . filter isDigit

takeLastDigit :: String -> Char
takeLastDigit = takeFirstDigit . reverse

-- part 2

createNum :: String -> Maybe Int
createNum xs = mkNum <$> findFirstNum xs <*> findLastNum xs
  where
    mkNum x y = x * 10 + y

nums :: [String]
nums =
  [ "one", "two", "three", "four"
  , "five", "six", "seven", "eight"
  , "nine"
  ]

strToInt :: Map String Int
strToInt = M.fromList $ zip nums [1..]

intToStr :: Map Int String
intToStr = M.fromList $ zip (M.elems strToInt) (M.keys strToInt)

findFirstNum :: String -> Maybe Int
findFirstNum [] = Nothing
findFirstNum (x:xs)
  | isDigit x
  , Just z <- M.lookup (digitToInt x) intToStr
  = M.lookup z strToInt
  | otherwise =
      case find (`isPrefixOf` (x:xs)) nums of
        Just y -> M.lookup y strToInt
        Nothing -> findFirstNum xs

findLastNum :: String -> Maybe Int
findLastNum = go . reverse
  where
    go [] = Nothing
    go (y:ys)
      | isDigit y
      , Just z <- M.lookup (digitToInt y) intToStr
      = M.lookup z strToInt
      | otherwise =
          case find (`isPrefixOf` (y:ys)) (reverse <$> nums) of
            Just z -> M.lookup (reverse z) strToInt
            Nothing -> go ys
