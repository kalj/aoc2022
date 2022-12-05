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

parseInput :: [Char] -> ([[Char]], [(Int,Int,Int)])
parseInput contents =
  (state, instructions)
  where
    state = parseState stateStr
    instructions = map (parseInstruction.words) $ lines instrStr
    parseInstruction tokens = (read (tokens !! 1) :: Int,
                             (read (tokens !! 3) :: Int) - 1,
                             (read (tokens !! 5) :: Int) - 1)
    [stateStr, instrStr] = splitOn "\n\n" contents

move :: [String] -> Int -> Int -> [String]
move oldstate isrc idst =
  pre2 ++ (newDst :post2)
  where
    newDst = dst++[last src]
    (pre2, dst:post2) = splitAt idst withoutMoved
    withoutMoved = pre1 ++ (init src :post1)
    (pre1, src:post1) = splitAt isrc oldstate

applyPart1Instruction :: [String] -> (Int,Int,Int) -> [String]
applyPart1Instruction state instruction =
  foldl instr state [1..nMove] 
  where
    instr oldstate _ = move oldstate iSrc iDst
    (nMove,iSrc, iDst) = instruction

moveStack :: [String] -> Int -> Int -> Int -> [String]
moveStack oldstate isrc idst n =
  pre2 ++ (dst++ toMove):post2
  where
    (pre2, dst:post2) = splitAt idst withoutMoved
    withoutMoved = pre1 ++ newSrc:post1
    (newSrc,toMove) = splitAt (length src - n) src
    (pre1, src:post1) = splitAt isrc oldstate

applyPart2Instruction :: [String] -> (Int,Int,Int) -> [String]
applyPart2Instruction state instruction =
  moveStack state iSrc iDst nMove
  where
    (nMove,iSrc, iDst) = instruction
    
part1 :: String -> String
part1 contents =
  map last finalState
  where
    finalState = foldl applyPart1Instruction initialState instructions
    (initialState, instructions) = parseInput contents

part2 :: String -> String
part2 contents = 
  map last finalState
  where
    finalState = foldl applyPart2Instruction initialState instructions
    (initialState, instructions) = parseInput contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
