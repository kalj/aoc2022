
{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List ( maximumBy )
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

vZipW :: (Int->Int->a) -> Vec4 -> Vec4 -> (a,a,a,a)
vZipW f (x1,x2,x3,x4) (y1,y2,y3,y4) = (f x1 y1, f x2 y2, f x3 y3, f x4 y4)

vMap :: (Int->a) -> Vec4 -> (a,a,a,a)
vMap f (x1,x2,x3,x4) = (f x1, f x2, f x3, f x4)

vAdd :: Vec4 -> Vec4 -> Vec4
vAdd = vZipW (+)

vSub :: Vec4 -> Vec4 -> Vec4
vSub = vZipW (-)

vScale :: Int -> Vec4 -> Vec4
vScale s = vMap (*s)

lstFromVec4 :: Vec4 -> [Int]
lstFromVec4 (x1,x2,x3,x4) = [x1,x2,x3,x4]

incDim :: Vec4 -> Int -> Vec4
incDim (x1,x2,x3,x4) 0 = (x1+1,x2,x3,x4)
incDim (x1,x2,x3,x4) 1 = (x1,x2+1,x3,x4)
incDim (x1,x2,x3,x4) 2 = (x1,x2,x3+1,x4)
incDim (x1,x2,x3,x4) _ = (x1,x2,x3,x4+1)

vGet :: Int -> Vec4 -> Int
vGet 0 (x,_,_,_) = x
vGet 1 (_,x,_,_) = x
vGet 2 (_,_,x,_) = x
vGet 3 (_,_,_,x) = x

affords :: Vec4 -> Vec4 -> Bool
affords (r1,r2,r3,r4) (c1,c2,c3,c4) = c1<=r1 && c2<=r2 && c3<=r3 && c4<=r4

data ProdState = ProdState { resourceCount :: Vec4
                           , robotCount :: Vec4
                           }
  deriving(Show)

geodeCount :: Vec4 -> Int
geodeCount = vGet 3

orderStates :: ProdState -> ProdState -> Ordering
orderStates s1 s2 = (geodeCount $ resourceCount s1) `compare` (geodeCount $ resourceCount s2)

-- stepTime :: Blueprint -> Int -> ProdState -> ProdState
-- stepTime _ 0 state = state
-- stepTime bp t state =
--   maximumBy orderStates $ map procOpt options

--   where
--     res = resourceCount state
--     robs = robotCount state

--     options = (res,robs):[(res `vSub` cost, incDim robs i) | (cost,i) <-zip bp [0..], affords res cost]
--     procOpt (rs,rb) = stepTime bp (t-1) state { robotCount = rb, resourceCount = rs `vAdd` robs }

stupid :: Blueprint -> Int -> ProdState -> Int
-- sample:
-- 17 -- 1.6 s
-- 18 -- 4.9 s
-- 19 -- 14.3 s
-- 20 -- 51.7 s
-- input:
-- 18 -- 13.1s
-- 20 -- 2m18s
stupid _ 0 state = geodeCount $ resourceCount state
stupid bp t state =
  maximum $ map procOpt options
  where
    res = resourceCount state
    robs = robotCount state

    options = (res,robs):[(res `vSub` cost, incDim robs i) | (cost,i) <-zip bp [0..], affords res cost]
    procOpt (rs,rb) = stupid bp (t-1) state { robotCount = rb, resourceCount = rs `vAdd` robs }

timeLeft :: Vec4 -> Vec4 -> Maybe Int
timeLeft v1 v2 = timeLeft' (lstFromVec4 v1) (lstFromVec4 v2)

timeLeft' :: [Int] -> [Int] -> Maybe Int
timeLeft' [] _ = Just 0
timeLeft' _ [] = Just 0
timeLeft' (c:cs) (r:rs)
  | c==0 =  timeLeft' cs rs
  | r==0 = Nothing
  | otherwise = case timeLeft' cs rs of
                  Nothing -> Nothing
                  Just mx -> Just (max mx tl)
                    where  tl = 1 + ((c-1) `div` r) -- integer div with round up

smart :: Blueprint -> Int -> ProdState -> Int
-- sample:
-- 18 -- 0.7 s
-- 19 -- 1.1 s
-- 20 -- 2.7s
-- 21 -- 9.3s
-- 22 -- 22.7s
-- 23 -- 1m12s
-- 24 -- 4m14s
-- input:
-- 18 -- 0.6s
-- 19 -- 1.2s
-- 20 -- 2.8s
-- 21 -- 8.2s
-- 22 -- 26.0s
-- 23 -- 1m23s
-- 24 -- 5m12s
smart bp t state =
  if null options then
    geodeCount $ res `vAdd` (t `vScale` robs)
  else
    maximum $ map procOpt options
  where
    res = resourceCount state
    robs = robotCount state

    options = mapMaybe getOption $ zip bp [0..]
    getOption :: (Vec4,Int) -> Maybe (Int,Int)
    getOption (cost,i) =
      let missingFunds = vMap (max 0) $ cost `vSub` res
      in timeLeft missingFunds robs >>= \tl -> if tl<t then Just (tl,i) else Nothing

    procOpt (tl,i) = smart bp (t-tl-1) state { robotCount = incDim robs i, resourceCount = (res `vAdd` ((1+tl) `vScale` robs)) `vSub` (bp!!i) }


computeMaxGeodes tLen bp =
  smart bp tLen ProdState { resourceCount = (0,0,0,0)
                          , robotCount = (1,0,0,0)
                          }

smartBounded :: [Vec4] -> Vec4 -> Int -> ProdState -> Int
-- sample:
-- 21 -- 4.7s
-- 22 -- 17.6s
-- 23 -- 1m1s
-- 24 -- 3m47s
-- 25 -- 14m49s
smartBounded bp maxRobs t state =
  if null options then
    geodeCount $ res `vAdd` (t `vScale` robs)
  else
    maximum $ map procOpt options
  where
    res = resourceCount state
    robs = robotCount state

    robotBound (_,i) = not (i/=3 && vGet i maxRobs == vGet i robs)

    options = mapMaybe getOption $ filter robotBound $ zip bp [0..]
    getOption :: (Vec4,Int) -> Maybe (Int,Int)
    getOption (cost,i) =
      let missingFunds = vMap (max 0) $ cost `vSub` res
      in timeLeft missingFunds robs >>= \tl -> if tl<t then Just (tl,i) else Nothing

    mkState tl i = state { robotCount = incDim robs i, resourceCount = (res `vAdd` ((1+tl) `vScale` robs)) `vSub` (bp!!i) }
    procOpt (tl,i) = smartBounded bp maxRobs (t-tl-1) $ mkState tl i

computeMaxGeodes2 tLen bp =
  smartBounded bp maxRobs tLen ProdState { resourceCount = (0,0,0,0)
                                         , robotCount = (1,0,0,0)
                                         }
  where maxRobs = foldl1 (vZipW max) bp

-- part1 :: String -> Int
part1 contents =
  let bps = map parseLine $ lines contents
      maxG = map (computeMaxGeodes 24) bps
      qlSum = sum $ zipWith (*) [1..] maxG
  in (maxG, qlSum)


-- part2 :: String -> Int
part2 contents =
  let bps = map parseLine $ lines contents
      maxG = map (computeMaxGeodes 25) $ take 3 bps
      gProd = product maxG
  in (maxG, gProd)

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
