import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.Char (ord, chr)
import Data.List (elemIndex, delete)
import Data.Maybe

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

data Map = Map [[Int]] Coord Coord

instance Show Map where
  show (Map heights sPos ePos) =
    unlines m
    where
      m = zipWith (\row i -> zipWith (\ch j-> trchr (i,j) ch) row [0..]) hm [0..]
      hm = map (map h2a) heights
      h2a h = chr $ ord 'a' + h
      trchr p ch
        | p==sPos   = 'S'
        | p==ePos   = 'E'
        | otherwise = ch


findChar :: Char -> [[Char]] -> Coord
findChar key =
  head . mapMaybe foo . zipWith (\i row -> (i, elemIndex key row)) [0..]
  where
    foo (i,Just j) = Just (i,j)
    foo (i,Nothing) = Nothing

parseMap :: String -> Map
parseMap contents =
  Map heights startPos endPos
  where
    charmap = lines contents
    heights = map (map a2h) charmap
    a2h ch = ord (trchr ch) - ord 'a'
      where
        trchr ch
          | ch=='E'   = 'z'
          | ch=='S'   = 'a'
          | otherwise = ch
    startPos = findChar 'S' charmap
    endPos = findChar 'E' charmap

(!!!) m (ci,cj) = m !! ci !! cj

setAt m (ci,cj) v =
  preRows++(newRow:postRows)
  where
    newRow = preCols++(v:postCols)
    (preCols,_:postCols) = splitAt cj myRow
    (preRows,myRow:postRows) = splitAt ci m

type AccF = (Coord -> Coord -> Bool)
  
accessibleFrom :: Map -> AccF
accessibleFrom (Map hs _ _) src dst =
  dstH <= srcH+1
  where
    srcH = hs!!!src
    dstH = hs!!!dst

getAccessibleNeighbors :: AccF -> Map -> Coord -> [Coord]
getAccessibleNeighbors accF (Map hs _ _) n@(i,j) =
  filter fltr [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
  where
    fltr c = inside c && accF n c
    inside (ci,cj) = ci>=0 && cj>=0 && ci < length hs && cj < length (head hs)

unvisitedNeighbors :: AccF -> Coord -> [Coord] -> Map -> [Coord]
unvisitedNeighbors accF n unvisited m =
  filter (`elem` unvisited) $ getAccessibleNeighbors accF m n

-- showGrid :: Show a => [[a]] -> String
-- showGrid = unlines . map show

-- traceGrid g = trace (showGrid g) g

dijkstra :: AccF -> Coord -> Coord -> [[Int]] -> [Coord] -> Map -> [[Int]]
dijkstra accF tgt curr ds unvis m =
  if tgt == nextCurr then
    nextDs
  else
    dijkstra accF tgt nextCurr nextDs nextUnvis m
  where
    nbrs = unvisitedNeighbors accF curr unvis m
    currD = ds!!!curr
    nextDs = foldl updateDistance ds nbrs
    updateDistance dsi nbr = setAt dsi nbr newDist
      where newDist = min (dsi!!!nbr) (currD+1)

    nextCurr = foldl1 (\u1 u2 -> if (nextDs!!!u1) < (nextDs!!!u2) then u1 else u2) unvis
    nextUnvis = delete nextCurr unvis

replicate2 n m v = replicate n (replicate m v)

distMap m@(Map hs sp ep) =
  distances
  where
    distances = dijkstra (accessibleFrom m) ep curr0 dist0 unvis0 m
    curr0 = sp
    dist0 = setAt (replicate2 rows cols (rows*cols)) sp 0
    unvis0 = [(i,j) | i<-[0..rows-1], j<-[0..cols-1], (i,j)/=sp]
    rows = length hs
    cols = length $ head hs

part1 :: [[Int]] -> Map -> Int
part1 ds m =
  ds !!! sp
  where
    (Map _ sp ep) = m


getHeightPoints hs h =
  [(i,j) | i<-[0..rows-1], j<-[0..cols-1], hs!!!(i,j) == h]
  where
    rows = length hs
    cols = length $ head hs

revDistMap :: Map -> [[Int]]
revDistMap m@(Map hs sp ep) =
  dijkstra accfun sp curr0 dist0 unvis0 m
  where
    accfun s d = accessibleFrom m d s
    curr0 = ep
    dist0 = setAt (replicate2 rows cols (rows*cols)) ep 0
    unvis0 = [(i,j) | i<-[0..rows-1], j<-[0..cols-1], (i,j)/=ep]
    rows = length hs
    cols = length $ head hs

part2 :: [[Int]] -> Map -> Int
part2 dists m =
  minimum $ map (dists!!!) aPts
  where
    aPts = getHeightPoints hs 0
    (Map hs _ ep) = m

main :: IO ()
main = do
  contents <- getArgContents
  let m = parseMap contents
      distances = revDistMap m
  let part1answer = part1 distances m
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 distances m
  putStrLn $ "Part2 answer:\n" ++ show part2answer
