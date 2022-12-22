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

incDim :: Vec4 -> Int -> Vec4
incDim (x1,x2,x3,x4) 0 = (x1+1,x2,x3,x4)
incDim (x1,x2,x3,x4) 1 = (x1,x2+1,x3,x4)
incDim (x1,x2,x3,x4) 2 = (x1,x2,x3+1,x4)
incDim (x1,x2,x3,x4) _ = (x1,x2,x3,x4+1)

affords :: Vec4 -> Vec4 -> Bool
affords (r1,r2,r3,r4) (c1,c2,c3,c4) = c1<=r1 && c2<=r2 && c3<=r3 && c4<=r4

geodeCount (ProdState (_,_,_,g1) _) = g1

orderStates s1 s2 = (geodeCount s1) `compare` (geodeCount s2)

stepTime :: Blueprint -> Int -> ProdState -> ProdState
stepTime _ 0 state = state
stepTime bp t state =
  maximumBy orderStates $ map procOpt options

  where
    res = resourceCount state
    robs = robotCount state

    options = (res,robs):[(res `subVec4` cost, incDim robs i) | (cost,i) <-zip bp [0..], affords res cost]
    procOpt (rs,rb) = stepTime bp (t-1) state { robotCount = rb, resourceCount = rs `addVec4` robs }

maxGeode :: Blueprint -> Int -> ProdState -> Int
maxGeode _ 0 state = geodeCount state
maxGeode bp t state =
  maximum $ map procOpt options
  where
    res = resourceCount state
    robs = robotCount state

    options = (res,robs):[(res `subVec4` cost, incDim robs i) | (cost,i) <-zip bp [0..], affords res cost]
    procOpt (rs,rb) = maxGeode bp (t-1) state { robotCount = rb, resourceCount = rs `addVec4` robs }

computeMaxGeodes bp =
  -- stepTime bp 20 ProdState { resourceCount = (0,0,0,0)
  maxGeode bp 24 ProdState { resourceCount = (0,0,0,0)
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
