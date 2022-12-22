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

data Action = Move Int | TurnR | TurnL
  deriving(Show)

parseActions :: String -> [Action]
parseActions [] = []
parseActions s =
  case span isDigit s of
    ([],c:rest) -> (if c == 'R' then TurnR else TurnL) : parseActions rest
    (ds,rest)   -> Move (read ds) : parseActions rest

parseStuff contents =
  (grid,actions)
  where
    grid = lines gridStr
    actions = parseActions actionStr
    [gridStr,actionStr] = splitOn "\n\n" contents

type Grid = [String]
type Coord = (Int,Int)


data State = State { gridMap   :: Grid
                   , position  :: Coord
                   , direction :: Int
                   }
  deriving(Show)

at :: State -> Coord -> Char
at s c = (gridMap s)!!(fst c)!!(snd c)


wrapPos s (r,c) = case direction s of
  0 -> (r,findIndex (' '/=) (gridMap s!!r)
  1 -> (r,findIndex (' '/=) (gridMap s!!r)
  2 -> (r,findIndex (' '/=) (gridMap s!!r)
  _ -> (r,findIndex (' '/=) (gridMap s!!r)

takeStep :: State -> State
takeStep s = s { position = newPos }
  where
    dir = direction s
    oldPos@(r,c) = position s
    newPos = if s`at`movePos == '.' then movePos else oldPos
    movePos = if s`at`tgtPos == ' ' then wrapPos dir tgtPos else tgtPos
    tgtPos = case dir of
               0 -> (r+1,c)
               1 -> (r,c+1)
               2 -> (r-1,c)
               _ -> (r,c-1)

doAction :: State -> Action -> State
doAction s TurnR        = s { direction = (direction s + 1) `mod` 4 }
doAction s TurnL        = s { direction = (direction s - 1) `mod` 4 }
doAction s (Move steps) = (!!steps) $ iterate takeStep s

-- part1 :: String -> Int
part1 contents =
  foldl doAction State { gridMap = grid, position = startPos, direction = 0 } actions
  where
    startPos = (0,fromJust (elemIndex '.' firstRow))
    firstRow = head grid
    (grid,actions) = parseStuff contents

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
