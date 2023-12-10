module Main where

import           Data.Function
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe
import qualified Data.Set      as S
import           Debug.Trace   (traceShow)

main :: IO ()
main = do
  grid <- fromList (1,1) <$> readFile "10.txt"
--  grid <- fromList (1,1) <$> pure input
  let start = M.fromList (zip (M.elems grid) (M.keys grid)) M.! 'S'
  print (longestPath grid start `div` 2)

input :: String
input =
  ".....\n\
  \.S-7.\n\
  \.|.|.\n\
  \.L-J.\n\
  \....."

input2 :: String
input2 =
  "..F7.\n\
  \.FJ|.\n\
  \SJ.L7\n\
  \|F--J\n\
  \LJ..."

longestPath :: Grid -> (Int,Int) -> Int
longestPath grid start = go start 0 mempty
  where
    go c x seen
      | c == start && x > 0 = x
      | c `S.member` seen = x
      | otherwise = do
          let vals =
                [ go n (x+1) (S.insert c seen `S.difference` S.singleton start)
                | n <- S.toList (S.fromList (getNeighbors grid c) `S.difference` seen)
                ]
          maximum (0 : vals)

getNeighbors :: Grid -> (Int,Int) -> [(Int,Int)]
getNeighbors grid (x,y) =
  case M.lookup (x,y) grid of
    Just '|' -> [ north, south ]
    Just '-' -> [ east, west ]
    Just 'L' -> [ north, east ]
    Just '7' -> [ south, west ]
    Just 'J' -> [ north, west ]
    Just 'F' -> [ south, east ]
    Just '.' -> [ ]
    Just 'S' -> [ north , south, east, west, ne, se, sw, nw ]
    Nothing -> [ ]
  where
    south = (x,y+1)
    north = (x,y-1)
    east  = (x+1,y)
    west  = (x-1,y)

    se = (x+1, y+1)
    ne = (x+1, y-1)
    sw = (x-1, y+1)
    nw = (x-1, y-1)

type Grid = M.Map (Int,Int) Char

fromList :: (Int, Int) -> String -> Grid
fromList c str = snd (foldl' go (c, M.empty) str)
  where
    go :: ((Int,Int), Grid) -> Char -> ((Int,Int), Grid)
    go ((x,y), m) '\n' = ((1, y+1), m)
    go ((x,y), m) s = ((x+1, y), M.insert (x,y) s m)
