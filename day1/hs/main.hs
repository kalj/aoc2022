import System.Environment
import System.Exit
import System.IO
import Data.List.Split
import Data.List

computeSums :: String -> [Int]
computeSums fileContents =
  map splitAndSum parts
  where
    splitAndSum part = sum [ read y | y <- splitOn "\n" part, length y > 0]
    parts = splitOn "\n\n" fileContents

computeTopSum :: String -> Int
computeTopSum fileContents =
  sum $ take 3 $ reverse $ sort sums
  where
    sums = computeSums fileContents

main :: IO ()
main = do
  args <- getArgs
  if (length args) < 1
    then die "Insufficient arguments"
    else return ()
  let filename = args !! 0
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  let topSum = computeTopSum contents
  putStrLn $ "top sum: " ++ show topSum
