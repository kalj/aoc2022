import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific

fromSnafu s =
  fromSnafu' s 0

fromSnafu' [] n = n
fromSnafu' (x:xs) n =
  let xDig = case x of
               '1' -> 1
               '2' -> 2
               '-' -> -1
               '=' -> -2
               -- '0' -> 0
               _ -> 0
  in fromSnafu' xs (5*n+xDig)

snafuDivMod n =
  let (q,r) = divMod n 5
  in if r < 3 then (q,r)
     else (q+1,r-5)

toSnafu 0 = ""
toSnafu n =
  let (q,r) = snafuDivMod n
  in (toSnafu q) ++
     [ case r of
         1 -> '1'
         2 -> '2'
         -1 -> '-'
         -2 -> '='
         -- 0 -> '0'
         _ -> '0'
     ]

parseSnafus contents = map fromSnafu . lines


-- part1 :: String -> Int
part1 contents =
  toSnafu $ sum $ parseSnafus contents
  where

part2 :: String -> Int
part2 contents =
  length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  -- putStrLn $ "Part1 answer:\n" ++ show part1answer
  putStrLn $ "Part1 answer:\n" ++ part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
