import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Text.Regex
-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

parseLine l = 
  case matchRegex (mkRegex "Sensor at x=(.*), y=(.*): .* is at x=(.*), y=(.*)") l of
    Just m -> ((sx, sy), dist)
      where
        dist = abs (bx-sx) + abs (by-sy)
        sx = read (m!!0)
        sy = read (m!!1)
        bx = read (m!!2)
        by = read (m!!3)
    Nothing -> error "failed parsing line"

sensorCoverage :: Int -> ((Int,Int),Int) -> Maybe (Int,Int)
sensorCoverage y ((sx,sy),d) =
  let dy = abs (y-sy)
      dx = d-dy
  in if dx < 0 then Nothing
     else Just (sx-dx,sx+dx)
  
sumCoverage :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
sumCoverage [] c  = [c]
sumCoverage cl@(cc:ccs) c = 
  let cb = fst c
      ce = snd c
      ccb = fst cc
      cce = snd cc
  in if ce < ccb
     then c:cl     -- strictly to the left, prepend to list
     else
       if cb > cce -- strictly to the right of head, add to tail
       then cc:sumCoverage ccs c
       else        -- overlap with ead exists merge with first, possibly also with rest
         sumCoverage ccs (min cb ccb, max ce cce)

coverageSize :: [(Int,Int)] -> Int  
coverageSize = sum . map (\(b,e)->e-b)

part1 :: String -> Int
part1 contents =
  coverageSize $ foldl sumCoverage [] $ mapMaybe (sensorCoverage y) sensorData
  where
    sensorData = map parseLine $ lines contents
    isSample = fst (fst (head sensorData)) == 2
    y = if isSample then 10 else 2000000

-- constrain (l,u) (b,e) = 
--   if newE>newB then Just (newB,newE) else Nothing
--   where 
--     newB = max l b
--     newE = min u e  

-- invert cov l u = 
--   mapMaybe (constrain (l,u)) cov


-- getAdmissible cov bound =

--   mapMaybe (constrain bound) cov
enumeratePoints :: [(Int,Int)] -> [Int]
enumeratePoints [] = []
enumeratePoints ((b,e):r) = [b..e]++enumeratePoints r

subtractCov :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
subtractCov (l,u) cov = []

-- part2 :: String -> Int
part2 contents = 
  concat [[(x,y) | x<-(enumeratePoints $ subtractCov (lowerBound,upperBound) (getCoverage y))]| y<-[lowerBound..upperBound]]
  where
    (lowerBound,upperBound) = if isSample then (0,20) else (0,4000000)
    getCoverage y = foldl sumCoverage [] $ mapMaybe (sensorCoverage y) sensorData
    sensorData = map parseLine $ lines contents
    isSample = fst (fst (head sensorData)) == 2

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ (unlines $ map show part2answer)
