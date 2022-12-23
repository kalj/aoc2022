import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.IO

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle
  
-- problem specific

type Coord = (Int,Int)  

-- dir : N>S>W>E>N>S>W>E>..  
--  <->  0>1>2>2>0>1>2>3>..  

data State = State { positions :: [Coord]
                   , preferredDir :: Int
                   }
  deriving(Show)

parse contents =
  let ls = lines contents
      elfPos = concat $ zipWith (\r row-> map (\c->(r,c)) $ elemIndices '#' row) [0..] ls
  in State { positions = elfPos, preferredDir = 0 }

diffuse s =
  let newPos = 

-- part1 :: String -> Int
part1 contents =
  let state = parse contents
  in diffuse state

part2 :: String -> Int
part2 contents =
  length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
