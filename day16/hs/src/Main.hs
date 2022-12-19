import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Regex
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.ST

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff
parseLine :: String -> (String, Int, [String])
parseLine l =
  case matchRegex (mkRegex "Valve (..) has flow rate=(.*); .* valves? (.*)$") l of
    Just m -> (nodeName, rate, connections)
      where
        nodeName = head m
        rate = read (m!!1)
        connections = splitOn ", " (m!!2)
    Nothing -> error "failed parsing line"

data Graph = Graph { nodes :: [(String,Int)]
                   , edges :: [(String,String,Int)]
                   }

instance Show Graph where
  show (Graph ns es) =
    "Nodes: "++ show ns ++ "\n"++
    "Edges: "++ show es -- (map (\(a,b,_)->fst (ns!!a) ++ " - " ++ fst (ns!!b)) es)

buildGraph :: [(String,Int,[String])] -> Graph
buildGraph rows = Graph ns es
  where
    ns = map (\(n,r,_)->(n,r)) rows
    -- lbl2idx n = fromJust $ findIndex (\(nn,_)->n==nn) ns
    consEdges (node,_,conns) = mapMaybe (\other-> if other>node then Just (node, other, 1) else Nothing) conns
    es = concatMap consEdges rows

getNeighbors :: Graph -> String -> [String]
getNeighbors (Graph _ es) n = mapMaybe f es
  where
    f (a,b,_)
     | n==a = Just b
     | n==b = Just a
     | otherwise = Nothing


pruneGraph :: Graph -> Graph
pruneGraph g@(Graph ns _) =
  Graph (ns\\nToRemove) (foldl pruneEdges (edges g) nToRemove)
  where
    nToRemove = filter (\(n,r) -> n/="AA" && r == 0 && length(getNeighbors g n)<=2) ns
    pruneEdges es (n,_) = without++[(if n==a1 then b1 else a1, if n==a2 then b2 else a2,w1+w2)]
      where
        (a1,b1,w1) = head eToRemove
        (a2,b2,w2) = eToRemove!!1
        (eToRemove,without) = partition (\(a,b,_)->a==n||b==n) es

parseGraph :: String -> Graph
parseGraph = pruneGraph . buildGraph . map parseLine . lines

type DistMatrix = Array (Int,Int) Int

allDistances :: Graph -> DistMatrix
allDistances (Graph ns es) =
  let maxDist = sum $ map (\(_,_,w)->w) es -- ~ INF
      nnodes = length ns
      lbl2idx n = fromJust $ findIndex (\(nn,_)->n==nn) ns
      idxEdges = map (\(a,b,w) -> (lbl2idx a, lbl2idx b, w)) es

  in runSTArray $ do
    marr <- newArray ((0,0),(nnodes-1,nnodes-1)) maxDist

    forM_ [0..(nnodes-1)] $ \i ->
        writeArray marr (i,i) 0

    forM_ idxEdges $ \(a,b,w) -> do
        writeArray marr (a,b) w
        writeArray marr (b,a) w

    forM_ [0..(nnodes-1)] $ \k ->
      forM_ [0..(nnodes-1)] $ \i ->
        forM_ [0..(nnodes-1)] $ \j -> do
          marrIK <- readArray marr (i,k)
          marrKJ <- readArray marr (k,j)
          marrIJ <- readArray marr (i,j)
          when (marrIK + marrKJ < marrIJ) $
            writeArray marr (i,j) (marrIK + marrKJ)
    return marr

data SearchState = SearchState Int Int [Int] [Int] DistMatrix

maxPressureHelper :: SearchState -> (Int,[(Int,Int)])
maxPressureHelper (SearchState curr tLeft toVisit rates distMat) =
  foldl procNode (0,[(curr,tLeft)]) toVisit
  where
    procNode (pmax,maxSeq) nxt =
      let tl = tLeft - distMat!(curr,nxt) - 1
          (pp,pseq) = maxPressureHelper (SearchState nxt tl nodesToVisit rates distMat)
          p = tl*(rates!!nxt) + pp
          nodesToVisit = [t|t<-toVisit, t/=nxt]
      in if tl > 0 && p > pmax then (p,(curr,tLeft):pseq) else (pmax,maxSeq)

seqToStr :: Int -> Graph -> [(Int, Int)] -> [String]
seqToStr startT grph = map (\(i,t)-> show (startT-t) ++ ":" ++ fst (nodes grph!!i))
-- seqToStr startT grph = map (\(i,t)-> show (startT-t) ++ ":" ++ show i)

getMaxPressure :: DistMatrix -> Graph -> (Int,[String])
getMaxPressure dists grph =
  (maxP, seqToStr startT grph idxSeq)
  where
    (maxP,idxSeq) = maxPressureHelper (SearchState startIdx startT toVisit rates dists)
    startIdx = fromJust $ findIndex (\(nn,_)->"AA"==nn) (nodes grph)
    startT = 30
    toVisit = [i | i<-[0..n-1],i/=startIdx]
    rates = [r | (_,r)<-nodes grph]
    n = length (nodes grph)

part1 :: String -> (Int, [String])
part1 contents =
  getMaxPressure dists grph
  where
    dists = allDistances grph
    grph = parseGraph contents

data SearchStateP2 = SearchStateP2 Int Int Int Int [Int] [Int] DistMatrix

maxPressureHelperP2 :: SearchStateP2 -> (Int,[(Int,Int)],[(Int,Int)])
maxPressureHelperP2 (SearchStateP2 meCurr eCurr meTimeLeft eTimeLeft toVisit rates distMat) =
  foldl procBranch (0,[(meCurr,meTimeLeft)],[(eCurr,eTimeLeft)]) options
  where
    options = [(Just m,Just e) | m<-meOpts, e<-eOpts, fst m /= fst e] ++
              [(Just m,Nothing) | m<-meOpts] ++
              [(Nothing,Just e) | e<-eOpts]
    meOpts = getOptions meTimeLeft meCurr
    eOpts = getOptions eTimeLeft eCurr
    getOptions tl c = mapMaybe (getOption tl c) toVisit
    getOption tl c n = if ntl>0 then Just (n,ntl) else Nothing
      where ntl = tl - distMat!(c,n)-1

    procBranch (pmax,maxSeqMe,maxSeqE) (me,e) =
      case (me,e) of
        (Just (meNxt,meTl),Just (eNxt,eTl)) ->
            let (pp,ppSeqMe,ppSeqE) = maxPressureHelperP2 (SearchStateP2 meNxt eNxt meTl eTl nodesToVisit rates distMat)
                p = meTl*(rates!!meNxt) + eTl*(rates!!eNxt) + pp
                nodesToVisit = [t|t<-toVisit, t/=meNxt && t/=eNxt]
            in if p > pmax then (p,(meCurr,meTimeLeft):ppSeqMe,(eCurr,eTimeLeft):ppSeqE) else (pmax,maxSeqMe,maxSeqE)
        (Just (meNxt,meTl),Nothing) ->
            let (pp,ppSeqMe) = maxPressureHelper (SearchState meNxt meTl nodesToVisit rates distMat)
                p = meTl*(rates!!meNxt) + pp
                nodesToVisit = [t|t<-toVisit, t/=meNxt]
            in if p > pmax then (p,(meCurr,meTimeLeft):ppSeqMe,maxSeqE) else (pmax,maxSeqMe,maxSeqE)
        (Nothing, Just (eNxt,eTl)) ->
            let (pp,ppSeqE) = maxPressureHelper (SearchState eNxt eTl nodesToVisit rates distMat)
                p = eTl*(rates!!eNxt) + pp
                nodesToVisit = [t|t<-toVisit, t/=eNxt]
            in if p > pmax then (p,maxSeqMe,(eCurr,eTimeLeft):ppSeqE) else (pmax,maxSeqMe,maxSeqE)


getMaxPressureP2 :: DistMatrix -> Graph -> (Int,[String], [String])
getMaxPressureP2 dists grph =
  (maxP, seqToStr startT grph meIdxSeq, seqToStr startT grph elphIdxSeq)
  where
    (maxP,meIdxSeq,elphIdxSeq) = maxPressureHelperP2 (SearchStateP2 startIdx startIdx startT startT [i | i<-[0..n-1],i/=startIdx] rates dists)
    startIdx = fromJust $ findIndex (\(nn,_)->"AA"==nn) (nodes grph)
    startT = 26
    n = length (nodes grph)
    rates = [r | (_,r)<-nodes grph]

part2 :: String -> (Int, [String], [String])
part2 contents =
  getMaxPressureP2 dists grph
  where
    dists = allDistances grph
    grph = parseGraph contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++  show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
