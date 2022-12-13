import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List.Split (splitOn)

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

parse = map lines . splitOn "\n\n"

part1 contents = parse contents

part2 contents = length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
