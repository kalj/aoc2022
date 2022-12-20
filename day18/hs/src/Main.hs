import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.List

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle


parseLine :: String -> [Int]
parseLine l = map read (splitOn "," l)

areNeighbors xs ys =
  1 == sum (zipWith (\x y-> abs (x-y)) xs ys)


totalNeighbors :: [[Int]] -> Int
totalNeighbors [] = 0
totalNeighbors (cb:cbs) = length (filter (areNeighbors cb) cbs) + totalNeighbors cbs


part1 contents =
  (length stuff)*6 - (neighbors*2)
  where
    neighbors = totalNeighbors stuff
    stuff = map parseLine $ lines contents

-- part2 :: String -> (Int, [String], [String])
part2 contents = length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++   show part1answer
  -- putStrLn $ "Part1 answer:\n" ++  unlines (map show part1answer)
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
