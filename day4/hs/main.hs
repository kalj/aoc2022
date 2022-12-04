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


parseRange :: String -> (Int,Int)
parseRange str =
  (read beg, read end)
  where
  [beg,end] = splitOn "-" str

containedIn :: (Int,Int) -> (Int,Int) -> Bool
containedIn r1 r2 =
  beg1>=beg2 && end1<=end2
  where
    (beg1,end1) = r1
    (beg2,end2) = r2

hasFullyContained :: String -> Bool
hasFullyContained line =
  containedIn r1 r2 || containedIn r2 r1
  where
    r1 = parseRange p1
    r2 = parseRange p2
    [p1,p2] = splitOn "," line

part1 :: String -> Int
part1 contents =
  length $ filter hasFullyContained ls
  where
  ls = lines contents


overlaps :: (Int,Int) -> (Int,Int) -> Bool
overlaps r1 r2 =
  beg1<=end2 && end1>=beg2
  where
    (beg1,end1) = r1
    (beg2,end2) = r2


lineHasOverlap :: String -> Bool
lineHasOverlap line =
  overlaps r1 r2
  where
    r1 = parseRange p1
    r2 = parseRange p2
    [p1,p2] = splitOn "," line


part2 :: String -> Int
part2 contents =
  length $ filter lineHasOverlap ls
  where
  ls = lines contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
