import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Debug.Trace

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

newtype Coord = Coord (Int,Int) deriving(Show, Eq, Ord)
xcoord (Coord (x,_)) = x
ycoord (Coord (_,y)) = y

data RopeState = RopeState {
  nodes :: [Coord],
  tipTrail :: S.Set Coord
  }


moveTailPiece :: Coord -> Coord -> Coord
moveTailPiece hp tp =
  res 
  where
    xdiff = xcoord hp - xcoord tp
    ydiff = ycoord hp - ycoord tp
    res = 
      if abs xdiff == 2 then
        Coord (xcoord tp + div xdiff 2, 
         if abs ydiff > 0 then
           ycoord tp + signum ydiff
         else
           ycoord tp
        )
      else if abs ydiff == 2 then
             Coord (if abs xdiff > 0 then
                xcoord tp + signum xdiff 
               else
                xcoord tp,
               ycoord tp + div ydiff 2
             )
           else tp

moveHead :: Char -> Coord -> Coord
moveHead dir hp =   
  case dir of
    'U' -> Coord (xcoord hp, ycoord hp + 1)
    'D' -> Coord (xcoord hp, ycoord hp - 1)
    'R' -> Coord (xcoord hp + 1, ycoord hp)
    _   -> Coord (xcoord hp - 1, ycoord hp) -- 'L'
      
  
applyMovement :: Char -> RopeState -> RopeState
applyMovement dir inState =
  RopeState { nodes = newNodes
            , tipTrail = S.insert (last newNodes) (tipTrail inState)
            }
  where
    newHp = moveHead dir $ head $ nodes inState
    newNodes = newHp:newTail
    (_, newTail) = foldl foo (newHp,[]) oldTail
    foo (prev,done) next = (moved,done++[moved])
      where moved = moveTailPiece prev next
    oldTail = tail $ nodes inState


processCommands :: RopeState -> [String] -> RopeState
processCommands  =
  foldl processCmd
  where
    processCmd inState cmd =
      iterate (applyMovement dir) inState !! len
      where
        dir = head cmd
        [_, lenStr] = splitOn " " cmd
        len = read lenStr

traceTrail :: Int -> [String] -> S.Set Coord
traceTrail len =
  tipTrail . processCommands initialState 
  where 
    initialState = RopeState { nodes = replicate len $ Coord (0,0)
                             , tipTrail = S.singleton $ Coord (0,0)
                             }
  
newtype TrailMap = TrailMap (S.Set Coord)
instance Show TrailMap where
  show (TrailMap s) = show $ toGrid s

instance Show RopeState where
  show (RopeState rope trail) =
    show ropeGrid
    where
      ropeGrid = foldl (\g (ch,pos) -> addToGrid ch g pos) trailGrid $ reverse $ zip "H123456789" rope
      trailGrid = toGrid trail
      

addToGrid :: Char -> Grid -> Coord -> Grid
addToGrid c (Grid rows mn@(Coord (xmin,ymin)) mx@(Coord (xmax,ymax))) (Coord (x,y)) =
  Grid (prerows++newrow:postrows) mn mx 
  where
    (prerows,row:postrows) = splitAt (y-ymin) rows
    newrow = zipWith (\i e -> if i==x then c else e ) [xmin..] row
  
data Grid = Grid [String] Coord Coord
instance Show Grid where
  show (Grid rows _ _) = unlines $ reverse rows
  
toGrid :: S.Set Coord -> Grid
toGrid trail =
  addToGrid 's' finalGrid (Coord (0,0))
  where
    finalGrid = foldl (addToGrid '#') initGrid $ S.toList trail
    xmax = maximum $ map xcoord $ S.toList trail
    ymax = maximum $ map ycoord $ S.toList trail
    xmin = minimum $ map xcoord $ S.toList trail
    ymin = minimum $ map ycoord $ S.toList trail
    -- xmin = -30
    -- xmax = 30
    -- ymin = -30
    -- ymax = 30
    initGrid = Grid (replicate (ymax-ymin+1) $ replicate (xmax-xmin+1) '.') (Coord (xmin,ymin)) (Coord (xmax,ymax))

part1 :: String -> Int
part1 contents =
  length trail
  where
    trail = traceTrail 2 cmds
    cmds = lines contents
          

part2 :: String -> Int
part2 contents =
  length trail
  where
    trail = traceTrail 10 cmds
    cmds = lines contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
