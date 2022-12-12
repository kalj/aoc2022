import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.Char (ord, chr)
import Data.List (findIndex, elemIndex)
import Data.Maybe
import qualified Data.Set as S
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

data Map = Map [[Int]] (Int,Int) (Int,Int)

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


findChar :: Char -> [[Char]] -> (Int,Int)
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

getNeighbors n@(i,j) (Map hs _ _) =
  filter inside [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
  where
    inside (ci,cj) = ci>=0 && cj>=0 && ci < length hs && cj < length (head hs)

accessibleFrom hs src dst =
  dstH <= srcH+1
  where
    srcH = hs!!!src
    dstH = hs!!!dst

getDoneNeighborsWithAccess n ds m@(Map hs _ _) =
  filter fltr $ getNeighbors n m
  where
    fltr c = done c && accessibleFrom hs c n
    done c = ds!!!c /= -1

getNextNodes n ds m@(Map hs _ _) =
  filter fltr $ getNeighbors n m
  where
    fltr c = not (done c) && accessibleFrom hs n c
    done c = ds!!!c /= -1

computeNodeDistance ds n m =
  setAt ds n myDist
  where
    myDist = minimum candDs + 1
    cands = getDoneNeighborsWithAccess n ds m
    candDs = map (ds!!!) cands

showGrid :: Show a => [[a]] -> String
showGrid = unlines . map show

traceGrid g = trace (showGrid g) g

computeDistances :: [[Int]] -> S.Set (Int,Int) ->  Map -> [[Int]]
computeDistances ds nset m
  | S.null nset = ds
  | otherwise = computeDistances  ds2 ns2 m
  where
    ds2 =  computeNodeDistance ds (trace (show n ++ "  " ++ show (length nset)) n) m
    (n:ns) = S.toList nset
    ns2 = S.union (S.fromList ns) (S.fromList (getNextNodes n ds2 m))

-- part1 :: String -> Int
-- part1 :: String -> [[Int]]
part1 contents =
  distances !!! ep
  where
    distances = computeDistances startDs startNodes mapp
    mapp@(Map hs sp ep) = parseMap contents
    startDs = setAt allMinusOnes sp 0
    startNodes = S.fromList $ getNextNodes sp startDs mapp
    allMinusOnes = replicate (length hs) (replicate (length (head hs)) (-1))

part2 :: String -> Int
part2 contents = length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
