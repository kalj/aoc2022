import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle


data Jet = LeftJ | RightJ
  deriving(Show)

parseJetChar :: Char -> Jet
parseJetChar '>' = RightJ
parseJetChar '<' = LeftJ
parseJetChar c = error ("Invalid Jet char"++show c)


data JetQueue = JetQueue {jets:: [Jet]
                         , nextJet :: Int
                         }
  deriving(Show)

getNextJet :: JetQueue -> (Jet, JetQueue)
getNextJet q = ( jets q !! nj, q {nextJet = rem (nj+1) (length (jets q))})
  where nj = nextJet q

parseJet :: String -> JetQueue
parseJet str = JetQueue { jets = map parseJetChar str
                        , nextJet = 0
                        }

data Piece = Horiz | Plus | RevL | Vert | Cube
  deriving(Show)

nextPiece :: Piece -> Piece
nextPiece Horiz = Plus
nextPiece Plus = RevL
nextPiece RevL = Vert
nextPiece Vert = Cube
nextPiece Cube = Horiz

pieceWidth :: Piece -> Int
pieceWidth Horiz = 4
pieceWidth Plus = 3
pieceWidth RevL = 3
pieceWidth Vert = 1
pieceWidth Cube = 2

getPieceRows :: Piece -> [String]
getPieceRows p = case p of
                   Horiz -> ["@@@@"]
                   Plus  -> [".@.","@@@",".@."]
                   RevL  -> ["@@@","..@","..@"]
                   Vert  -> ["@","@","@","@"]
                   Cube  -> ["@@","@@"]

padToHeight :: [[Char]] -> Int -> [[Char]]
padToHeight ch h =
  let currH = length ch
  in if h>currH then ch++replicate (h-currH) (replicate 7 '.')
     else ch

data ChamberState = ChamberState { chamber :: [[Char]]
                                 , currentPiece :: Piece
                                 , currentPiecePos :: (Int,Int)
                                 , queue :: JetQueue
                                 , stuckCount :: Int
                                 }

padPieceRow :: [String] -> Int -> String -> String
padPieceRow rs currCol l =  replicate currCol '.'++ l ++ replicate (7-currCol-length (head rs)) '.'

instance Show ChamberState where
  show s = "ChamberState { chamber = " ++ show (chamber s) ++
           ", currentPieceIdx = " ++ show (currentPiece s) ++
           ", currentPiecePos = " ++ show (currentPiecePos s) ++ "}\n\n" ++
           unlines (reverse c)
    where
      c = " +-------+" : map (\r->' ':'|':r++"|") (chm ++ padding ++ pieceRows)
      pieceRows = padPieceRows $ getPieceRows $ currentPiece s
      padPieceRows rs = map (padPieceRow rs currCol) rs
      (currRow,currCol) = currentPiecePos s

      chm = chamber s
      padding = replicate (currRow-nRows) emptyRow
      emptyRow = replicate 7 '.'
      nRows = length (chamber s)

blockedAt :: ChamberState -> (Int, Int) -> Bool
blockedAt s (r,c) =
  let chm = chamber s
      currH = length chm
  in r < currH && (chm!!r)!!c /='.'

setAt :: [[a]] -> (Int, Int) -> a -> [[a]]
setAt m (r,c) v =
  zipWith (\rr row-> zipWith (\cc vv -> if cc==c && rr==r then v else vv) [0..] row) [0..] m


processChamber :: ChamberState -> ChamberState
processChamber s =
  let (j,nextJQ) = getNextJet (queue s)
      (currRow, currCol) = currentPiecePos s
      currPc = currentPiece s
      maxCol = 7 - pieceWidth currPc
      -- process Jet
      tgtCol = case j of
                 LeftJ -> currCol-1
                 RightJ -> currCol+1
      newCol = if tgtCol<0 || tgtCol>maxCol || blockedAt s (currRow,newCol) then currCol
               else tgtCol

      -- process fall
      newRow  = currRow-1
      pr = getPieceRows currPc
      stuck = currRow==0 || or (zipWith (\r row ->or (zipWith (\c v-> v=='@' && blockedAt s (r,c)) [newCol..] row)) [newRow..] pr)

  in if stuck
     then let newChmbr = foldl addBlock paddedChmbr blocks
              blocks = concat (zipWith (\r row->zipWith (\c v-> (r,c,v)) [newCol..] row) [currRow..] pr)
              paddedChmbr = padToHeight (chamber s) (currRow + length pr)
              addBlock ch (r,c,v) = if v=='@'
                                    then setAt ch (r,c) '#'
                                    else ch
          in s { chamber = newChmbr
               , currentPiece = nextPiece currPc
               , currentPiecePos = (length newChmbr + 3,2)
               , queue = nextJQ
               , stuckCount = stuckCount s + 1
               }
     else s { currentPiecePos = (newRow, newCol)
            , queue = nextJQ
            }

heightAtNStuck :: Int -> ChamberState -> Int
heightAtNStuck nStuck =
  length.chamber . until (\ss->stuckCount ss == nStuck) processChamber

-- part1 :: String -> (Int, [String])
part1 contents =
  heightAtNStuck 2022 startState
  -- take 25 $ iterate processChamber startState
  where
    startState = ChamberState { chamber = []
                              , currentPiece = Horiz
                              , currentPiecePos = (3, 2) -- row, col
                              , queue = jetQueue
                              , stuckCount = 0
                              }
    jetQueue = parseJet contents

-- part2 :: String -> (Int, [String], [String])
part2 contents = length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++  show part1answer
  -- putStrLn $ "Part1 answer:\n" ++  unlines (map show part1answer)
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
