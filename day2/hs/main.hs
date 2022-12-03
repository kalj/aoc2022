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


getPart1RoundScore :: String -> String -> Int
getPart1RoundScore a b =
  point1+point2
  where
    point1 = myHand+1
    point2 = if myHand == opponentHand then 3
             else if myHand == (opponentHand+1) `rem` 3 then 6
             else 0
    opponentHand = case a of
      "A" -> 0
      "B" -> 1
      "C" -> 2
      _ -> error "failure"
    myHand = case b of
      "X" -> 0
      "Y" -> 1
      "Z" -> 2
      _ -> error "failure"

computeScore :: String -> (String -> String -> Int)-> Int
computeScore fileContents roundScoreFunc =
  sum $ map splitAndGetScore ls
  where
    splitAndGetScore line =
      roundScoreFunc a b
      where
        [a,b] = splitOn " " line
    ls = lines fileContents

getPart2RoundScore :: String -> String -> Int
getPart2RoundScore a b =
  handPoints+resultPoints
  where
    resultPoints = case b of
      "X" -> 0
      "Y" -> 3
      "Z" -> 6
      _ -> error "failure"
    opponentHand = case a of
      "A" -> 0
      "B" -> 1
      "C" -> 2
      _ -> error "failure"
    myHand = if resultPoints == 0 then (opponentHand+2) `rem` 3
             else if resultPoints == 3 then opponentHand
             else (opponentHand+1) `rem` 3
    handPoints = myHand+1

main :: IO ()
main = do
  contents <- getArgContents
  let part1score = computeScore contents getPart1RoundScore
  putStrLn $ "Part1 Score: " ++ show part1score
  let part2score = computeScore contents getPart2RoundScore
  putStrLn $ "Part2 Score: " ++ show part2score
