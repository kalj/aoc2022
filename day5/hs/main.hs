import Control.Monad (when)
import Data.List (transpose)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

parseState :: String -> [String]
parseState stateStr =
  map (takeWhile (/= ' ')) $ transpose $ map (take nCols . (++ repeat ' ')) rawRows
  where
    nCols = length $ head rawRows
    rawRows = reverse $ map mySplit itemLs
    mySplit (x : y : xs) = y : mySplit (drop 2 xs)
    mySplit _ = []
    itemLs = take (length ls - 1) ls
    ls = lines stateStr

parseInput :: [Char] -> ([[Char]], [String])
parseInput contents =
  (state, instructions)
  where
    state = parseState stateStr
    instructions = lines instrStr
    [stateStr, instrStr] = splitOn "\n\n" contents

applyInstruction :: [String] -> String -> [String]
applyInstruction state instruction =
  state
  where
    nMove = read (tokens !! 1) :: Int
    iSrc = (read (tokens !! 3) :: Int) - 1
    iDst = (read (tokens !! 5) :: Int) - 1
    tokens = words instruction

part1 :: String -> String
part1 contents =
  map last finalState
  where
    finalState = foldl applyInstruction initialState instructions
    (initialState, instructions) = parseInput contents

part2 :: String -> String
part2 contents = "banan"

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
