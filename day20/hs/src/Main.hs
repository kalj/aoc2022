import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.List
import Data.Maybe

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific
extractAt i ls =
  let (pre,e:post) = splitAt i ls
  in (e,pre++post)

insertAt i e ls =
  let (pre,post) = splitAt i ls
  in pre++e:post

wrap i n
    = i `mod` (n-1)

  -- | i < 0 = trace ("wrapping "++show i++" to "++show (i+(n-1))) (i+(n-1))
  -- | i>=n = trace ("wrapping "++show i++" to "++show (i-(n-1))) (i-(n-1))
  -- | otherwise = i

computePos :: Int -> Int -> Int -> Int
computePos old shft n =
  let newPos = wrap (old + shft) n
  in newPos
    -- if newPos < old && newPos == 0 then n - 1
    -- else if newPos>old && newPos == n-1 then 0
    -- else newPos

traceLst ls =
  trace (show (map snd ls)) ls

moveInd :: [(Int,Int)] -> Int -> [(Int,Int)]
moveInd ps i =
  let currPos = fromJust $ findIndex (\(ii,_) -> ii==i) ps
      (e,weE) = extractAt currPos ps
      insPos = computePos currPos (snd e) (length ps)
  in insertAt insPos e weE

doMixing lst =
  let indices = [0..(length lst)-1]
      pairs = zip indices lst
  in map snd $ foldl moveInd pairs indices

grooveCoord ls =
  let idxOf0 = fromJust $ elemIndex 0 ls
      valAt i = ls !! (i `mod` (length ls))
  in sum $ map valAt [idxOf0+1000,idxOf0+2000,idxOf0+3000]

-- part1 :: String -> Int
part1 contents =
  grooveCoord $ doMixing lst
  where
    lst :: [Int]
    lst = map read $ lines contents

doPart2Mixing lst =
  let indices = [0..(length lst)-1]
      pairs = zipWith (\i v->(i,811589153*v)) indices lst
      finalList = (!!10) $ iterate (\p-> foldl moveInd p indices) pairs
  in map snd finalList

part2 :: String -> Int
part2 contents =
  grooveCoord $ doPart2Mixing lst
  where
    lst :: [Int]
    lst = map read $ lines contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
