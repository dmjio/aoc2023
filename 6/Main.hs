import Data.List

main :: IO ()
main = do
  let
    xs = fmap f
      $ transpose
      $ fmap (fmap (read @Int) . tail . words)
      $ lines input

    f [x,y] = (x,y)

  print $ product (length . wins <$> xs)

wins :: (Int, Int) -> [Int]
wins (time, distance) = takeWhile (> distance) $ go <$> [ start 1 .. ]
  where
    go x = x * (time - x)

    start x
      | x * (time - x) <= distance = start (x + 1)
      | otherwise = x

input =
  "Time:      7  15   30\n\
  \Distance:  9  40  200"

input2 =
  "Time:        42     68     69     85\n\
  \Distance:   284   1005   1122   1341"

input3 =
  "Time:      71530\n\
  \Distance:  940200"

input4 =
  "Time:        42686985\n\
  \Distance:    284100511221341"
