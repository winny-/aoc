import Data.List

differences :: Eq a => [a] -> [a] -> Int
differences a b = sum $ zipWith (\x y -> if x == y then 1 else 0) a b

part1 :: [String] -> Int
part1 xs = let
  freq = map ((map length) . group . sort) xs
  twice = length $ filter (elem 2) freq
  thrice = length $ filter (elem 3) freq
  in twice * thrice

main = do
  w <- lines <$> getContents
  let sorted = sort w
  print $ part1 sorted
