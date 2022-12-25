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
  -- deriving(Show)
instance Show State
  where show s = "pos=" ++ show (position s) ++ "\n" ++
                 "dir=" ++ show (direction s) ++ "\n" ++
                 unlines (zipWith (\row r-> zipWith (renderPos r) row [0..]) (gridMap s) [0..])
          where
            renderPos r v c = if (r,c)==position s then
                                case direction s of
                                  0 -> '>'
                                  1 -> 'v'
                                  2 -> '<'
                                  _ -> '^'
                              else v


at :: State -> Coord -> Char
at s crd = (gridMap s)!!(fst crd)!!(snd crd)

isOutside :: State -> Coord -> Bool
isOutside s (r,c) = r<0 || r>=(length gd) || c<0 || c>= length (gd!!r)
  where gd = gridMap s


revFindIndex :: (a -> Bool) -> [a] -> Maybe Int
revFindIndex f xs = fmap ((length xs - 1) - )  (findIndex f (reverse xs))

wrapPos :: State -> Coord -> Coord
wrapPos s (r,c) = case direction s of
                    0 -> (r,fromJust $ findIndex (' '/=) (gridMap s!!r))
                    1 -> (fromJust (findIndex matchRow (gridMap s)), c)
                    2 -> (r, fromJust $ revFindIndex (' '/=) (gridMap s!!r))
                    _ -> (fromJust (revFindIndex matchRow (gridMap s)), c)
  where matchRow row = c < length row && row!!c /= ' '

takeStep :: State -> State
takeStep s = s { position = newPos }
  where
    dir = direction s
    oldPos@(r,c) = position s
    newPos = if s`at`movePos == '.' then movePos else oldPos
    movePos = if isOutside s tgtPos || s`at`tgtPos == ' ' then wrapPos s tgtPos else tgtPos
    tgtPos = case dir of
               0 -> (r,c+1)
               1 -> (r+1,c)
               2 -> (r,c-1)
               _ -> (r-1,c)

doAction :: State -> Action -> State
doAction s TurnR        = s { direction = (direction s + 1) `mod` 4 }
doAction s TurnL        = s { direction = (direction s - 1) `mod` 4 }
doAction s (Move steps) = (!!steps) $ iterate takeStep s

computePassword :: State -> Int
computePassword (State _ (r,c) f) = 1000*(r+1)+4*(c+1)+f

part1 :: String -> Int
part1 contents =
  computePassword finalState
  where
    finalState = foldl doAction State { gridMap = grid, position = startPos, direction = 0 } actions
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
