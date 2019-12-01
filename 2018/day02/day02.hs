import Data.List
import Debug.Trace
import Control.Monad

trace' :: Show a => a -> a
trace' x = trace (show x) x

differences :: Eq a => [a] -> [a] -> Int
differences a b = sum $ zipWith (\x y -> if x == y then 1 else 0) a b

common :: Eq a => [a] -> [a] -> [a]
common a b = map (\(x,_) -> x) $ filter (\(x,y) -> x == y) $ zip a b
-- common a b = map (\(x,_) -> x) $ zipWith (==) a b

-- Takes sorted input.
part1 :: [String] -> Int
part1 xs = let
  freq = map ((map length) . group . sort) xs
  twice = length $ filter (elem 2) freq
  thrice = length $ filter (elem 3) freq
  in twice * thrice

prod :: Eq aa => [aa] -> [aa] -> [(aa, aa)]
prod as bs = [(a, b) | a <- as, b <- bs, a /= b]

-- Takes sorted input.
part2 :: [String] -> String
part2 xs = snd . head $ sortBy (\(x, _) (y, _) -> compare x y) $ map f $ prod xs xs
  where f (a,b) = (differences a b, common a b)

main = do
  w <- lines <$> getContents
  let xs = sort $ filter (not . null) w
  print $ part1 xs
  print $ part2 xs
