import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.Char(digitToInt)
import Debug.Trace (traceShowId)
import GHC.Base (VecElem(Int16ElemRep))

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

computeVisible :: [[Int]] -> [[Bool]]
computeVisible grid =
  foldl1 (zipWith (zipWith (||))) $ map ($ grid) [visibleTopDown,visibleBottomUp,visibleLeftRight,visibleRightLeft] 
  where 
    visibleLeftRight = map (snd . processRowWise) 
    visibleRightLeft = map (reverse . snd . processRowWise . reverse ) 
    visibleTopDown   = snd . foldl processRow (initHeight, [])  
    visibleBottomUp  = reverse . visibleTopDown . reverse 

    processRowWise = foldl processInRow (-1, [])
      where processInRow (mh,prev) next = (max mh next, prev++[next>mh])

    processRow (maxHeights,prevRows) row = (newMaxHeights,prevRows++[newRow])
      where
        newRow = zipWith (>) row maxHeights
        newMaxHeights = zipWith max row maxHeights
    initHeight = replicate (length $ head grid) (-1)
  
countTrue :: [[Bool]] -> Int  
countTrue boolGrid =
  sum $ map (sum . map fromEnum)  boolGrid

parseGrid = map (map digitToInt) . lines

part1 :: String -> Int
part1 contents = 
  countTrue $ computeVisible $ parseGrid contents

scenicScore :: [[Int]] -> [[Int]]
scenicScore grid = 
  [ [score i j| j<-take (length $ head grid)[0..]]| i<- take (length grid) [0..]]
  where
    score i j = product $ map viewingDistance [tail right, tail below, reverse left, reverse above]
      
      where 
        viewingDistance :: [Int] -> Int
        viewingDistance xs = min (length xs) (1 + length ( takeWhile (<myheight) xs))

        (above,below) = splitAt i mycol
        (left,right) = splitAt j myrow
        myrow = grid !! i
        mycol = map (!!j) grid
        myheight = myrow !! j

gridmax :: [[Int]] -> Int
gridmax =  maximum . map maximum

part2 :: String -> Int
part2 =  gridmax . scenicScore . parseGrid 

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++  show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
