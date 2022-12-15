import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List.Split
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

type Coord = (Int,Int)

readCoord :: String -> Coord
readCoord =
  tuplify2 . map read . splitOn ","
  where
    tuplify2 [x,y] = (x,y)
    tuplify2 _ = error "invalid list for tuplify2"

data Cave = Cave [[Char]] Coord Coord

instance Show Cave where
  show (Cave cv _ _) = unlines cv

rel :: Coord -> Coord -> Coord
(rel) (x,y) (x0,y0) = (x-x0,y-y0)

gridAt gd (cx,cy) = gd !! cy !! cx

setGridAt m (cx,cy) v =
  preRows++(newRow:postRows)
  where
    newRow = preCols++(v:postCols)
    (preCols,_:postCols) = splitAt cx myRow
    (preRows,myRow:postRows) = splitAt cy m


setAt :: Cave -> Coord -> Char -> Cave
setAt (Cave cv mn mx) c v = Cave (setGridAt cv (c `rel` mn) v) mn mx

at (Cave cv mn mx) c = gridAt cv (c `rel` mn)

inside :: Coord -> Cave -> Bool  
inside (x,y) (Cave _ (mnx,mny) (mxx,mxy)) = (x>=mnx) && (x<=mxx) && (y>=mny) && (y<=mxy)

outside :: Coord -> Cave -> Bool  
outside c cv = not $ inside  c cv

range :: Int -> Int -> [Int]  
range s e =
  if e < s then [s,(s-1)..e]
  else [s..e]

addSegment :: Cave -> (Coord,Coord) -> Cave
addSegment cv ((sx,sy),(ex,ey)) = 
  if sx == ex 
  then foldl (\ccv y->setAt ccv (sx,y) '#') cv $ range sy ey
  else foldl (\ccv x->setAt ccv (x,sy) '#') cv $ range sx ex

addPath :: Cave -> [Coord] -> Cave
addPath cv p = 
  foldl addSegment cv $ zip p (tail p)

createCave :: [[Coord]] -> Cave
createCave ps =
  let
    allPoints = concat ps ++ [startPos]
    minX = minimum $ map fst allPoints
    maxX = maximum $ map fst allPoints
    minY = minimum $ map snd allPoints
    maxY = maximum $ map snd allPoints
    bg = replicate (maxY-minY+1) $replicate (maxX-minX+1) '.'
    cave0 = setAt (Cave bg (minX,minY) (maxX,maxY)) startPos '+'
  in foldl addPath cave0 ps


pourSand :: Cave -> Coord -> Int -> Int
pourSand cave curr@(cx,cy) cnt =
  tryMoveNextSpace [(cx,cy+1), (cx-1,cy+1), (cx+1, cy+1)]
  -- if p outside: return count
  -- if p free: recurse with p as new sp
  -- if p blocked: continue loop
  -- if exit loop == all blocked:
  --    add to cv, recurse with new point, and count+1
  where
    tryMoveNextSpace :: [Coord] -> Int
    tryMoveNextSpace [] = pourSand (setAt cave curr 'o') startPos (cnt+1) -- blocked!
    tryMoveNextSpace (s:ss)
      | s `outside` cave = cnt
      | cave `at` s == '.' = pourSand cave s cnt
      | otherwise = tryMoveNextSpace ss


readPaths :: String -> [[Coord]]
readPaths =  map (map readCoord . splitOn " -> ") . lines

part1 :: String -> Int
part1 contents =
  pourSand cave startPos 0 
  where
    cave = createCave $ readPaths contents

addFloorPath :: [[Coord]] -> [[Coord]]  
addFloorPath ps =
  ps++[fp]
  where
    maxY = maximum $ map snd $ concat ps
    fp = [(fst startPos-floory,floory),(fst startPos + floory,floory)]
    floory = maxY+2

fillSand :: Cave -> Coord -> Int -> Int
fillSand cave curr@(cx,cy) cnt =
  tryMoveNextSpace [(cx,cy+1), (cx-1,cy+1), (cx+1, cy+1)]
  -- if p outside: error!
  -- if p free: recurse with p as new sp
  -- if p blocked: continue loop
  -- if exit loop == all blocked:
  --    add to cv, recurse with new point, and count+1
  where
    tryMoveNextSpace :: [Coord] -> Int
    tryMoveNextSpace [] = if curr == startPos then cnt+1
                          else fillSand (setAt cave curr 'o') startPos (cnt+1) -- blocked!
    tryMoveNextSpace (s:ss)
      | s `outside` cave = error "sand ended up outside of cave!"
      | cave `at` s == '.' = fillSand cave s cnt
      | otherwise = tryMoveNextSpace ss

-- part2 :: String -> Int
part2 contents = 
  fillSand cave startPos 0 
  where
    cave = createCave $ addFloorPath $ readPaths contents

startPos :: Coord
startPos = (500,0)

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
