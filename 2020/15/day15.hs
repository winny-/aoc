import qualified Data.HashMap.Strict as M
import Data.List.Split
import Data.List

type SeenMap = M.HashMap Integer Index
type LastInteger = Integer
type Index = Integer
type State = (Index, LastInteger, SeenMap)

part1Seq :: [Integer] -> [State]
part1Seq xs'@(x:xs) = start ++ infSeq
  where infSeq = (iterate g (last start))::[State]
        g z@(idx, n, seen) =
          (case M.lookup n seen of
            Nothing -> (idx+1, 0, M.insert n idx seen)
            Just idx' -> (idx+1, idx - idx', M.insert n idx seen))
        start = scanl' f (1, x, M.empty) xs
        f (idx, lst, m) i = (idx+1, i, M.insert lst idx m)


answer :: [Integer] -> Int -> Integer
answer xs n = k
  where (_, k, _) = st
        st = part1Seq xs !! n

part1 :: [Integer] -> Integer
part1 xs = answer xs 2020


-- This is a bit slow.  First bit finished quickly, but then this goes on for a
-- minute and a half... gobbles up 2 GiB of memory.
-- real	1m28.773s
part2 :: [Integer] -> Integer
part2 xs = answer xs 30000000

main :: IO ()
main = do
  line <- getLine
  let inputs = map (\x -> read x :: Integer) $ splitOn "," line
  -- putStrLn $ show $ inputs
  putStrLn $ show $ part1 inputs
  putStrLn $ show $ part2 inputs
