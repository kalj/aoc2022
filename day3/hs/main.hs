import System.Environment
import System.Exit
import System.IO
import Data.Char

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


getPrio :: Char -> Int
getPrio c =
  if c >= 'a' then (ord c) - (ord 'a') + 1
  else (ord c) - (ord 'A') + 27

getRepeatedChar :: String -> Char
getRepeatedChar line =
  (!! 0) $ filter (\e -> elem e half2) half1
  where
    half1 = take n2 line
    half2 = drop n2 line
    n2 = (length line) `div` 2

getRepeatedPrio :: String -> Int
getRepeatedPrio line = getPrio $ getRepeatedChar line

part1 :: String -> Int
part1 contents =
  sum $ map getRepeatedPrio ls
  where
    ls = lines contents

getTrippleChar :: [String] -> Char
getTrippleChar ls =
  (!!0) $ filter (\c -> all (any (c==)) ls) allchars
  where
    allchars = [c | c<-['a'..'z']++['A'..'Z']]

getBadgePrio :: [String] -> Int
getBadgePrio ls =
  getPrio $ getTrippleChar ls


partition :: Int -> [a] -> [[a]]
partition _ [] =  []
partition n xs =  (take n xs) : (partition n (drop n xs))

part2 :: String -> Int
part2 contents =
  sum $ map getBadgePrio groupLines
  where
    ls = lines contents
    groupLines = partition 3 ls


main :: IO ()
main = do
  contents <- getArgContents
  let part1score = part1 contents
  putStrLn $ "Part1 Score: " ++ show part1score
  let part2score = part2 contents
  putStrLn $ "Part2 Score: " ++ show part2score
