import Control.Monad
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)
import System.Exit        (die)
import System.IO          (hGetContents, openFile, IOMode(ReadMode) )
import qualified Data.Set as S
import Data.Char

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

cAdd (x1,x2) (y1,y2) = (x1+y1,x2+y2)

data Winds = Winds [[(Int,Bool)]] [[(Int,Bool)]]
  deriving(Show)


parseWinds contents =
  let ls = map (tail.init) $ tail $ init $ lines contents
      h = length ls
      w = length $ head ls
      winds = concatMap (\(r,row) -> mapMaybe (\(c,v) -> if v=='.' then Nothing else Just (r,c,v)) $ zip [0..] row) $ zip [0..] ls
      colW = map (\ci->[(r,v=='v') | (r,c,v) <- winds, (v=='v' || v=='^') && c==ci ]) [0..(w-1)]
      rowW = map (\ri->[(c,v=='>') | (r,c,v) <- winds, (v=='>' || v=='<') && r==ri ]) [0..(h-1)]
  in Winds colW rowW

data FindState = FindState Int Winds (S.Set Coord)
  -- deriving(Show)

instance Show FindState where
  show (FindState t (Winds colWinds rowWinds) ps) =
    let makeRow ri windRow = zipWith (makeChar ri windRow) [0..]  colWinds
        makeChar ri windRow ci windCol
          | (ri,ci) `S.member` ps = 'E'
          | null rw && null cw = '.'
          | null rw && length cw == 1 = head cw
          | null cw && length rw == 1 = head rw
          | otherwise = chr $ ord '0'+ length cw + length rw
          where rw = [if d then '>' else '<' | (c,d) <- windRow, c==ci]
                cw = [if d then 'v' else '^' | (r,d) <- windCol, r==ri]

        withWinds = zipWith makeRow [0..] rowWinds
    in unlines withWinds ++ "\n t = " ++ show t ++ " Es = " ++ show ps

findTarget :: Coord -> FindState  -> FindState
findTarget tgt (FindState t (Winds cW rW) ps) =
  let h = length rW
      w = length cW
      newRW = map (map (\(c,d) -> ((if d then c+1 else c-1)`mod`w,d))) rW
      newCW = map (map (\(r,d) -> ((if d then r+1 else r-1)`mod`h,d))) cW
      prune (r,c) = (r,c)==(-1,0) || (r,c)==(h,w-1) || ( r>=0 && c>=0 && r<h && c<w && all (\(wc,_)->wc/=c) (newRW!!r) && all (\(wr,_)->wr/=r) (newCW!!c))
      newPs = S.filter prune $ S.unions $ map getCandidates $ S.toList ps
      getCandidates p = S.fromList $ map (cAdd p) [(0,0), (-1,0),(1,0),(0,-1),(0,1)]
      nextState = FindState (t+1) (Winds newCW newRW) newPs
  in if tgt `S.member` newPs then nextState
     else
       findTarget tgt nextState
       -- findTarget tgt (trace ("findOut state=\n" ++ show nextState) nextState)

part1 :: String -> Int
part1 contents =
  let winds@(Winds cW rW) = parseWinds contents
      h = length rW
      w = length cW
      state0 = FindState 0 winds (S.fromList [(-1,0)])
      (FindState t _ _) = findTarget (h,w-1) state0
  in t

-- part2 :: String -> Int
part2 contents =
  let winds@(Winds cW rW) = parseWinds contents
      h = length rW
      w = length cW
      (FindState t1 w1 _) = findTarget (h,w-1) (FindState 0 winds (S.fromList [(-1,0)]))
      (FindState t2 w2 _) = findTarget (-1,0) (FindState 0 w1 (S.fromList [(h,w-1)]))
      (FindState t3 _ _) = findTarget (h,w-1) (FindState 0 w2 (S.fromList [(-1,0)]))
  in ([t1,t2,t3], t1+t2+t3)

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  -- putStrLn $ "Part1 answer:\n" ++  part1answer
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
