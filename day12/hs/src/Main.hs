import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.Char (ord, chr)
import Data.List (elemIndex)
import qualified Data.Heap as H
-- import qualified Data.Set as S
import qualified Data.HashSet as S
import Data.Maybe

type UnvisSet = S.HashSet Coord

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
    foo (_,Nothing) = Nothing

parseMap :: String -> Map
parseMap contents =
  Map heights startPos endPos
  where
    charmap = lines contents
    heights = map (map a2h) charmap
    a2h ch = ord (trchr ch) - ord 'a'
      where
        trchr c
          | c=='E'   = 'z'
          | c=='S'   = 'a'
          | otherwise = c
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

-- unvisitedNeighbors :: AccF -> Coord -> UnvisSet -> Map -> [Coord]
unvisitedNeighbors :: Coord -> DjkState -> [Coord]
unvisitedNeighbors c (DjkState af _ _ uv m _) =
  filter (`S.member` uv) $ getAccessibleNeighbors af m c

-- showGrid :: Show a => [[a]] -> String
-- showGrid = unlines . map show
-- traceGrid g = trace (showGrid g) g

data DjkState = DjkState {accF      :: AccF
                         ,targetPos :: Coord
                         ,distances :: [[Int]]
                         ,unvisited :: UnvisSet
                         ,mapp      :: Map
                         ,queue     :: H.MinPrioHeap Int Coord
                         }

getNext :: DjkState -> Maybe (Coord, DjkState)
getNext s =
  case H.view (queue s) of
    Nothing -> Nothing
    Just ((_,c), nextq) ->
      if c == targetPos s then
        Nothing
      else if visited c then
        getNext s { queue = nextq }
      else
        Just (c, s {unvisited = S.delete c (unvisited s), queue = nextq })
  where visited = not . (`S.member` unvisited s)
  
getDist s c = distances s !!! c

dijkstra :: Coord -> DjkState -> [[Int]]
dijkstra curr s =
  case getNext updatedS of
    Just (nextCurr, nexts) -> dijkstra nextCurr nexts
    Nothing -> distances updatedS
  where
    newDist = getDist s curr + 1
    updatedS = foldl updateDistance s (unvisitedNeighbors curr s)
    updateDistance ss nbr = if newDist < getDist  ss nbr
                                    then ss { distances = setAt (distances ss) nbr newDist, queue = newq }
                                    else ss
        where newq = H.insert (newDist,nbr) (queue ss)

replicate2 :: Int -> Int -> a -> [[a]]
replicate2 n m v = replicate n (replicate m v)

part1 :: [[Int]] -> Map -> Int
part1 ds m =
  ds !!! sp
  where
    (Map _ sp _) = m


getHeightPoints :: [[Int]] -> Int -> [Coord]
getHeightPoints hs h =
  [(i,j) | i<-[0..rows-1], j<-[0..cols-1], hs!!!(i,j) == h]
  where
    rows = length hs
    cols = length $ head hs

revDistMap :: Map -> [[Int]]
revDistMap m@(Map hs sp ep) =
  dijkstra curr0 (DjkState accfun sp dist0 unvis0 m uvq0)
  where
    accfun s d = accessibleFrom m d s
    curr0 = ep
    dist0 = setAt (replicate2 rows cols (rows*cols)) ep 0
    unvis0list = [(i,j) | i<-[0..rows-1], j<-[0..cols-1], (i,j)/=ep]
    unvis0 = S.fromList unvis0list
    uvq0 = H.fromList $ map (\c->(dist0!!!c,c)) unvis0list
    rows = length hs
    cols = length $ head hs

part2 :: [[Int]] -> Map -> Int
part2 dists m =
  minimum $ map (dists!!!) aPts
  where
    aPts = getHeightPoints hs 0
    (Map hs _ _) = m

main :: IO ()
main = do
  contents <- getArgContents
  let m = parseMap contents
      dists = revDistMap m
  let part1answer = part1 dists m
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 dists m
  putStrLn $ "Part2 answer:\n" ++ show part2answer
