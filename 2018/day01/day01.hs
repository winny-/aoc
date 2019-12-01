import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set

stringsToInts :: [String] -> [Int]
stringsToInts = map read

part1 :: [Int] -> Int
part1 = foldl (+) 0

part2 :: [Int] -> Int
part2 xs = f (Set.singleton 0) 0 $ cycle xs
  where f st n (x:xs) = let m = x+n
                        in if m `Set.member` st
                           then m
                           else f (Set.insert m st) m xs

main = do
  w <- words . filter (\x -> not $ x `elem` "+,") <$> getContents
  let numbers = stringsToInts w
  printf "Part #1: %d\n" $ part1 numbers
  printf "Part #2: %d\n" $ part2 numbers
