import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.List
import Data.Maybe

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific

type Blueprint = [Vec4]

parseLine :: String -> Blueprint
parseLine ln = [(read (tokens!!6),0,0,0)
               , (read (tokens!!12),0,0,0)
               , (read (tokens!!18), read (tokens!!21), 0, 0)
               , (read (tokens!!27), 0,read (tokens!!30), 0)
               ]
  where
    tokens = words ln

type Vec4 = (Int,Int,Int,Int)

data ProdState = ProdState { resourceCount :: Vec4
                           , robotCount :: Vec4
                           }
  deriving(Show)
addVec4 :: Vec4 -> Vec4 -> Vec4
addVec4 (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1+y1,x2+y2,x3+y3,x4+y4)

subVec4 :: Vec4 -> Vec4 -> Vec4
subVec4 (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1-y1,x2-y2,x3-y3,x4-y4)

scaleVec4 :: Vec4 -> Int -> Vec4
scaleVec4 (x1,x2,x3,x4) s = (x1*s,x2*s,x3*s,x4*s)

lstFromVec4 (x1,x2,x3,x4) = [x1,x2,x3,x4]

addDim (x1,x2,x3,x4) 0 n = (x1+n,x2,x3,x4)
addDim (x1,x2,x3,x4) 1 n = (x1,x2+n,x3,x4)
addDim (x1,x2,x3,x4) 2 n = (x1,x2,x3+n,x4)
addDim (x1,x2,x3,x4) 3 n = (x1,x2,x3,x4+n)

affords :: Vec4 -> Vec4 -> Bool
affords (r1,r2,r3,r4) (c1,c2,c3,c4) = c1<=r1 && c2<=r2 && c3<=r3 && c4<=r4

nAffords :: Vec4 -> Vec4 -> Int
nAffords r c =
  trace (show r ++ " " ++ show c ++ "  ->  " ++ show banan) banan
  where minMax mm (rr,cc) =
          if cc == 0 then mm
          else min mm (rr `div` cc)
        banan = foldl minMax 10000 $ zip (lstFromVec4 r) (lstFromVec4 c)

purchaseOptions :: Vec4 -> Blueprint -> [Vec4]
purchaseOptions res bp =
  map snd $ foldl getOpts [(res,(0,0,0,0))] $ zip [0..] bp
  where
    getOpts opts (i,price) =  concatMap (expandOpts i price) opts
    expandOpts i price (resLeft,opt) = map (\nbuy -> (subScale resLeft nbuy price, addDim opt i nbuy)) [0..(nAffords resLeft opt)]
    subScale x a y = x `subVec4` (scaleVec4 y a)

orderStates :: ProdState -> ProdState -> Ordering
orderStates (ProdState (_,_,_,g1) _) (ProdState (_,_,_,g2) _) = g1 `compare` g2

computeCost :: Blueprint -> Vec4 -> Vec4
computeCost bp po = foldl1 addVec4 $ zipWith scaleVec4  bp (lstFromVec4 po)

stepTime :: Blueprint -> Int -> ProdState -> ProdState
stepTime bp 0 state = state
stepTime bp t state =
  maximumBy orderStates finalStates
  where
    res = resourceCount state
    robs = robotCount state
    -- nextStates = mstate]
    genNextState po = state { robotCount = robs `addVec4` po, resourceCount = res `addVec4` robs `subVec4` computeCost bp po }
    finalStates = map (stepTime bp (t-1).genNextState) (purchaseOptions res bp)


computeMaxGeodes bp =
  stepTime bp 1 ProdState { resourceCount = (0,0,0,0)
                           , robotCount = (1,0,0,0)
                           }

-- part1 :: String -> Int
part1 contents =
  let bps = map parseLine $ lines contents
  in map computeMaxGeodes bps


-- part2 :: String -> Int
part2 contents =
  length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  -- putStrLn $ "Part1 answer:\n" ++ unlines (map show part1answer)
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
