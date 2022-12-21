import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S
import Data.Sequence ((|>), ViewL( (:<) ))
import qualified Data.Sequence as Sq

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

type Coord = (Int,Int,Int)

lstToCoord :: [Int] -> Coord
lstToCoord ls = (x1,x2,x3)
  where
    x1 = ls!!0
    x2 = ls!!1
    x3 = ls!!2

coordToLst :: Coord -> [Int]
coordToLst (x1,x2,x3) = [x1,x2,x3]

addCoord (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3)

subCoord (x1,x2,x3) (y1,y2,y3) = (x1-y1,x2-y2,x3-y3)

zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord op c1 c2 = lstToCoord $ zipWith op (coordToLst c1) (coordToLst c2)

allRel op (x1,x2,x3) (y1,y2,y3) = op x1 y1 && op x2 y2 && op x3 y3

parseLine :: String -> Coord
parseLine l = lstToCoord $ map read (splitOn "," l)

areNeighbors :: Coord -> Coord -> Bool
areNeighbors xs ys =
  1 == sum (zipWith (\x y-> abs (x-y)) (coordToLst xs) (coordToLst ys))

totalNeighbors :: [Coord] -> Int
totalNeighbors [] = 0
totalNeighbors (cb:cbs) = length (filter (areNeighbors cb) cbs) + totalNeighbors cbs

countExposedSides :: [Coord] -> Int
countExposedSides cubes =
  6 * (length cubes) - (2*nbs)
  where
    nbs = totalNeighbors cubes

part1 :: String -> Int
part1 contents =
  countExposedSides cubes
  where
    cubes = map parseLine $ lines contents

data Volume = Volume { minCoord :: Coord
                     , maxCoord :: Coord
                     , filled :: [[[Bool]]]
                     }
  deriving(Show)
(!!!) :: Volume -> Coord -> Bool
(!!!) v c = filled v!!r1!!r2!!r3
  where
    (r1,r2,r3) = c `subCoord ` minCoord v

setAt :: Volume -> Coord -> Bool -> Volume
setAt v c t = v { filled = zipWith replaceSlice [x01..] (filled v)
                }
  where
    replaceSlice i1 = zipWith (replaceRow i1) [x02..]
    replaceRow i1 i2 = zipWith (replaceVal i1 i2) [x03..]
    replaceVal i1 i2 i3 old = if c==(i1,i2,i3) then t else old
    (x01,x02,x03) = minCoord v

inside :: Volume -> Coord -> Bool
inside vol c =
  allRel (>=) c (minCoord vol) && allRel (<=) c (maxCoord vol)


emptyVol :: Coord -> Coord -> Volume
emptyVol minc maxc = Volume minc maxc filld
  where
    (s1, s2, s3) = (maxc `subCoord` minc) `addCoord` (1,1,1)
    filld = replicate s1 $ replicate s2 $ replicate s3 False

neighbors :: Volume -> Coord -> [Coord]
neighbors vol c =
  let candidates = [ c `subCoord` (1,0,0)
                   , c `addCoord` (1,0,0)
                   , c `subCoord` (0,1,0)
                   , c `addCoord` (0,1,0)
                   , c `subCoord` (0,0,1)
                   , c `addCoord` (0,0,1)
                   ]
  in filter (inside vol) candidates


data MyQueue = MyQueue (S.Set Coord) (Sq.Seq Coord)
  deriving(Show)

qnull :: MyQueue -> Bool
qnull (MyQueue s _) =  S.null s

emptyq :: MyQueue
emptyq = MyQueue S.empty Sq.empty

deque :: MyQueue -> (Coord,MyQueue)
deque (MyQueue s q) =
  case Sq.viewl q of
     (c :< newq) ->(c, MyQueue (S.delete c s) newq)
     _ -> error "Trying to deque empty queue"

enque :: MyQueue -> Coord -> MyQueue
enque mq@(MyQueue s q) c = if S.member c s then mq
else MyQueue (S.insert c s) (q|>c)

append :: MyQueue -> [Coord] -> MyQueue
append = foldl enque

doFill vol cset cq =
  if qnull cq then vol
  else let (c, cqN) = deque cq
           volN = setAt vol c True
           fillPts = filter (\cc-> not(S.member cc cset || (vol!!!cc))) $ neighbors vol c
           cqNN = append cqN fillPts
       in doFill volN cset cqNN

fillVolume :: [Coord] -> Volume
fillVolume cubes =
  let minPt = foldl1 (zipCoord min) cubes `subCoord` (1,1,1)
      maxPt = foldl1 (zipCoord max) cubes `addCoord` (1,1,1)
      startVol = emptyVol (traceShowId minPt) (traceShowId maxPt)
      cubeSet = S.fromList cubes
  in doFill startVol cubeSet (enque emptyq minPt)

enclosedCubes :: Volume -> [Coord]
enclosedCubes vol =
  let (x01,x02,x03) = minCoord vol
      zipRow x1 x2 v3 = mapMaybe (\(x3,fld)->if fld then Nothing else Just (x1,x2,x3)) $ zip [x03..] v3
      zipSlice x1 v2 = concat (zipWith (zipRow x1) [x02..] v2)
  in concat (zipWith zipSlice [x01..] (filled vol))

part2 :: String -> Int
part2 contents =
  countExposedSides (enclosedCubes vol)
  where
    vol = fillVolume cubes
    cubes = map parseLine $ lines contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++   show part1answer
  -- putStrLn $ "Part1 answer:\n" ++  unlines (map show part1answer)
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
