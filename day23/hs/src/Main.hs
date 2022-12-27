{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.ST
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

data Dir = North | South | West | East
  deriving(Show)

nextDir North = South
nextDir South = West
nextDir West = East
nextDir East = North

data State = State { positions :: [Coord]
                   , preferredDir :: Dir
                   }
  -- deriving(Show)
instance Show State where
  show (State pos _) =
    let grid = buildGrid pos
        ((rMin,cMin), (rMax,cMax)) = bounds grid
        ls = map (\r-> map (\c-> if grid!(r,c) then '#' else '.') [cMin..cMax]) [rMin..rMax]
        padded = map (\l -> '.':l++['.']) (emptyl:ls++[emptyl])
          where emptyl = replicate (length (head ls)) '.'
    in unlines padded

parse contents =
  let ls = lines contents
      elfPos = concat $ zipWith (\r row-> map (r,) $ elemIndices '#' row) [0..] ls
  in State { positions = elfPos, preferredDir = North }

type Grid = Array Coord Bool

getBoundsForCoords cs =
  let minPos = foldl1 (\(r1,c1) (r2,c2) -> (min r1 r2, min c1 c2)) cs
      maxPos = foldl1 (\(r1,c1) (r2,c2) -> (max r1 r2, max c1 c2)) cs
  in (minPos,maxPos)


buildGrid :: [Coord] -> Grid
buildGrid cs =
  let (minPos,maxPos) = getBoundsForCoords cs
  in runSTArray $ do
    marr <- newArray (minPos, maxPos) False
    forM_ cs $ \c -> writeArray marr c True
    return marr


notAlone grid (_,p@(r,c)) =
  let neighbors = [(rr,cc) | rr<-[r-1..r+1], cc<-[c-1..c+1], (rr,cc) /= p]
  in not $ all (isFree grid) neighbors


probePoints :: Coord -> Dir -> [Coord]
probePoints (r,c) North = [(r-1,c-1),(r-1,c),(r-1,c+1)]
probePoints (r,c) South = [(r+1,c-1),(r+1,c),(r+1,c+1)]
probePoints (r,c) West  = [(r-1,c-1),(r,c-1),(r+1,c-1)]
probePoints (r,c) East  = [(r-1,c+1),(r,c+1),(r+1,c+1)]

moveDir :: Dir -> Coord -> Coord
moveDir North (r,c) = (r-1,c)
moveDir South (r,c) = (r+1,c)
moveDir West  (r,c) = (r,c-1)
moveDir East  (r,c) = (r,c+1)

outOfBounds :: (Coord,Coord) -> Coord -> Bool
outOfBounds ((rMin,cMin), (rMax,cMax)) (r,c) = r<rMin || r>rMax || c<cMin || c>cMax

isFree :: Array Coord Bool -> Coord -> Bool
isFree grid p = outOfBounds (bounds grid) p || not (grid!p)

createMovement :: Array Coord Bool -> [Dir] -> (Int, Coord) -> Maybe (Int,Coord)
createMovement _ [] _ = Nothing
createMovement grid (d:ds) (i,p) =
  let pts = probePoints p d
  in if all (isFree grid) pts then Just (i, moveDir d p)
     else createMovement grid ds (i,p)

replaceAtIdx :: [a] -> Int -> a -> [a]
replaceAtIdx xs i x =
  let (pre,_:post) = splitAt i xs
  in pre++x:post

diffuse (State pos pdir) =
  let grid = buildGrid pos
      dirs = take 4 $ iterate nextDir pdir
      movements = mapMaybe (createMovement grid dirs ) $ filter (notAlone grid) $ zip [0..] pos
      filteredMovements = filter (\(i,m)-> not $ any (\(ii,mm)->m==mm && i/=ii) movements) movements
      applyMovement ps (i,p) = replaceAtIdx ps i p
  in State { preferredDir = nextDir pdir
           ,  positions = foldl applyMovement pos filteredMovements
           }

countEmptyTiles (State pos _) =
  let ((rMin,cMin), (rMax,cMax)) = getBoundsForCoords pos
      nTot = (rMax+1-rMin)*(cMax+1-cMin)
  in nTot - length pos

-- part1 :: String -> Int
part1 contents =
  let state = parse contents
      finalState = (!!11) $ iterate diffuse state
  in (countEmptyTiles finalState, finalState)

diffuseUntilConvergence n s@(State pos pdir) =
  let grid = buildGrid pos
      dirs = take 4 $ iterate nextDir pdir
      movements = mapMaybe (createMovement grid dirs ) $ filter (notAlone grid) $ zip [0..] pos
      filteredMovements = filter (\(i,m)-> not $ any (\(ii,mm)->m==mm && i/=ii) movements) movements
      applyMovement ps (i,p) = replaceAtIdx ps i p
  in if null filteredMovements then (n,s)
     else diffuseUntilConvergence (n+1) State { preferredDir = nextDir pdir
                                              ,  positions = foldl applyMovement pos filteredMovements
                                              }

-- part2 :: String -> Int
part2 contents =
  let state = parse contents
      (n,finalState) = diffuseUntilConvergence 1 state
  in (finalState, n)

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
