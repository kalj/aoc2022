import System.Environment
import System.Exit
import System.IO
import Data.List.Split

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

parseInstructions instrStr = lines instrStr

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

parseState stateStr =
  transpose $ reverse $ map mySplit itemLs
  where
    mySplit (x:y:xs) = y : mySplit (drop 2 xs)
    mySplit _ =  []
    itemLs = take ((length ls)-1) ls
    ls = lines stateStr

parseInput contents =
  (state,instructions)
  where
    state = parseState stateStr
    instructions = parseInstructions instrStr
    [stateStr,instrStr] = splitOn "\n\n" contents


-- part1 :: String -> String
part1 contents = parseInput contents

part2 :: String -> String
part2 contents = "banan"

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
