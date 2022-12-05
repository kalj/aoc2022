import System.Environment
import System.Exit
import System.IO

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

part1 :: String -> Int
part1 contents = length contents

part2 :: String -> Int
part2 contents = length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
