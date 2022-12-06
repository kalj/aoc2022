import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Data.List

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  if (length args) < 1
    then die "Insufficient arguments"
    else return ()
  let filename = args !! 0
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return contents

-- problem specific stuff

beginsWithMarker :: Int -> String -> Bool
beginsWithMarker len msg =
  len == (length $ nub potentialMarker)
  where
    potentialMarker = take len msg

findMarker len buf =
  head $ [n+len|n <- [0..length buf], msgBeginsWithMarker n]
  where
    msgBeginsWithMarker n = beginsWithMarker len (drop n buf)

part1 :: String -> Int
part1 contents = findMarker 4 contents

part2 :: String -> Int
part2 contents = findMarker 14 contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
