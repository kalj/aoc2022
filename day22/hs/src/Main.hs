{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.IO

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific

data Action = Move Int | TurnR | TurnL
  deriving(Show)

parseActions :: String -> [Action]
parseActions [] = []
parseActions s =
  case span isDigit s of
    ([],c:rest) -> (if c == 'R' then TurnR else TurnL) : parseActions rest
    (ds,rest)   -> Move (read ds) : parseActions rest

parseStuff :: [Char] -> ([String], [Action])
parseStuff contents =
  (grid,actions)
  where
    grid = lines gridStr
    actions = parseActions actionStr
    [gridStr,actionStr] = splitOn "\n\n" contents

type Grid = [String]
type Coord = (Int,Int)

data MapWalkState = MapWalkState { gridMap   :: Grid
                                 , position  :: Coord
                                 , direction :: Int
                                 }
  -- deriving(Show)
instance Show MapWalkState
  where show s = "pos=" ++ show (position s) ++ "\n" ++
                 "dir=" ++ show (direction s) ++ "\n" ++
                 unlines (zipWith (\row r-> zipWith (renderPos r) row [0..]) (gridMap s) [0..])
          where
            renderPos r v c = if (r,c)==position s then
                                case direction s of
                                  0 -> '>'
                                  1 -> 'v'
                                  2 -> '<'
                                  _ -> '^'
                              else v

at :: MapWalkState -> Coord -> Char
at s crd = (gridMap s)!!(fst crd)!!(snd crd)

isOutside :: MapWalkState -> Coord -> Bool
isOutside s (r,c) = r<0 || r>=(length gd) || c<0 || c>= length (gd!!r)
  where gd = gridMap s


revFindIndex :: (a -> Bool) -> [a] -> Maybe Int
revFindIndex f xs = fmap ((length xs - 1) - )  (findIndex f (reverse xs))

wrapPos :: MapWalkState -> Coord -> Coord
wrapPos s (r,c) = case direction s of
                    0 -> (r,fromJust $ findIndex (' '/=) (gridMap s!!r))
                    1 -> (fromJust (findIndex matchRow (gridMap s)), c)
                    2 -> (r, fromJust $ revFindIndex (' '/=) (gridMap s!!r))
                    _ -> (fromJust (revFindIndex matchRow (gridMap s)), c)
  where matchRow row = c < length row && row!!c /= ' '

takeStep :: MapWalkState -> MapWalkState
takeStep s = s { position = newPos }
  where
    dir = direction s
    oldPos@(r,c) = position s
    newPos = if s`at`movePos == '.' then movePos else oldPos
    movePos = if isOutside s tgtPos || s`at`tgtPos == ' ' then wrapPos s tgtPos else tgtPos
    tgtPos = case dir of
               0 -> (r,c+1)
               1 -> (r+1,c)
               2 -> (r,c-1)
               _ -> (r-1,c)

doAction :: MapWalkState -> Action -> MapWalkState
doAction s TurnR        = s { direction = (direction s + 1) `mod` 4 }
doAction s TurnL        = s { direction = (direction s - 1) `mod` 4 }
doAction s (Move steps) = (!!steps) $ iterate takeStep s

getPassword :: MapWalkState -> Int
getPassword (MapWalkState _ p f) = computePassword p f

computePassword :: Coord -> Int -> Int
computePassword (r,c) f = 1000*(r+1)+4*(c+1)+f

getStartPos g = (0,fromJust (elemIndex '.' (head g)))

part1 :: String -> Int
part1 contents =
  getPassword finalState
  where
    finalState = foldl doAction MapWalkState { gridMap = grid, position = startPos, direction = 0 } actions
    startPos = getStartPos grid
    (grid,actions) = parseStuff contents


data Side = Side { upSide :: (Int,Int)
                 , downSide :: (Int,Int)
                 , leftSide :: (Int,Int)
                 , rightSide :: (Int,Int)
                 , gridPosition :: Coord
                 , localMap :: [String]
                 }
  deriving(Show)

cAdd (x1,x2) (y1,y2) = (x1+y1,x2+y2)

data Neighbor = Absolute Int | Relative (Int,Int)

mapply f (a1,a2,a3,a4) = (f a1, f a2, f a3, f a4)

data Dir3 = Xpos | Xneg | Ypos | Yneg | Zpos | Zneg
  deriving(Show,Eq)

type Face = (Int, Dir3, Dir3)

inList x xs = isJust ( elemIndex x xs)
inQueue i xs = isJust ( find (\(j,_,_)->i==j) xs)
faceDone i fcs  = isJust ( find (\(j,_,_)->i==j) fcs)

data Offset = UpO | DownO | LeftO | RightO
  deriving(Show)

wOffset (r,c) UpO    = (r-1,c)
wOffset (r,c) DownO  = (r+1,c)
wOffset (r,c) LeftO  = (r,c-1)
wOffset (r,c) RightO = (r,c+1)

neg :: Dir3 -> Dir3
neg Xpos = Xneg
neg Xneg = Xpos
neg Ypos = Yneg
neg Yneg = Ypos
neg Zpos = Zneg
neg Zneg = Zpos

cross :: Dir3 -> Dir3 -> Dir3
cross Xpos Ypos = Zpos
cross Ypos Zpos = Xpos
cross Zpos Xpos = Ypos

cross Ypos Xpos = Zneg
cross Zpos Ypos = Xneg
cross Xpos Zpos = Yneg

cross Xneg a = neg $ cross Xpos a
cross Yneg a = neg $ cross Ypos a
cross Zneg a = neg $ cross Zpos a
cross a Xneg = neg $ cross a Xpos
cross a Yneg = neg $ cross a Ypos
cross a Zneg = neg $ cross a Zpos

relRot :: Dir3 -> Dir3 -> Dir3 -> Int
relRot expected actual axis
  | expected == actual = 0                -- expected is actual up, no rotation
  | expected == neg actual = 2            -- expected is actual down, rotate 180 deg
  | expected == (actual `cross` axis) = 1 -- expected is actual right, rotate 270 deg cw
  | expected == (axis `cross` actual) = 3 -- expected is actual left, rotate 90 deg cw
  | otherwise = error ("Invalid input to relRot: " ++ show expected ++ ", " ++ show actual ++ ", " ++ show axis)

doRot :: Offset -> (Dir3, Dir3) -> (Dir3, Dir3)
doRot UpO (norm,up) = (up,neg norm)
doRot DownO (norm,up) = (neg up, norm)
doRot LeftO (norm,up) = (norm `cross` up, up)
doRot RightO (norm,up) = (up `cross` norm,up)

buildFaces :: [[(Int,Offset)]] -> [(Int,Offset,Int)] -> [Face] -> [Face]
buildFaces _ [] faces = faces
buildFaces nl ((idx,offset,parent):queue) faces =
  let (_,pNor,pUp) = fromJust $ find (\(i,_,_)->i==parent) faces
      (norm,up) = doRot offset (pNor,pUp)
      ns = [(i,o,idx) | (i,o) <- nl!!idx, not (faceDone i faces) && not (inQueue i queue)]
  in
     buildFaces nl (queue++ns) (faces++[(idx,norm,up)])


toCubeGrid :: [String] -> [Side]
toCubeGrid g =
  let nRows = length g
      nCols = maximum $ map length g
      (rSides,cSides) =  if nRows `div` 3 == nCols `div` 4
                         then (3,4)
                         else (4,3)
      sideSize = nRows `div` rSides
      sideRows :: [(Int,[(Int,[String])])]
      sideRows = zipWith (\r rows -> (r,zip [0..] $ padSides $ transpose $ map (chunksOf sideSize) rows)) [0..] $ chunksOf sideSize g
      padSides sds = sds ++ replicate (cSides - length sds) emptySide
        where
          emptySide = replicate sideSize (replicate sideSize ' ')
      sidesRaw = concatMap (\(r,row) -> mapMaybe (\(c,sd)->if sd!!0!!0 /=' ' then Just ((r,c),sd) else Nothing) row)  sideRows

      neighborList = map getNeighbors sidesRaw
        where getNeighbors (mc,_) = mapMaybe (\ofs -> (,ofs) <$> findIndex (\(m,_) -> m == mc `wOffset` ofs) sidesRaw) [UpO,DownO,LeftO,RightO]

      faceDirs = map (\(_,a,b)->(a,b)) $ sortOn (\(i,_,_)->i) $ buildFaces neighborList queue0 faces0
        where
          queue0 = [(i,o,0) | (i,o) <- head neighborList]
          faces0 = [(0, Zpos, Ypos)]

      makeSide (norm,up) ((r,c),lm) =
        Side { upSide    = getSide up (neg norm)
             , downSide  = getSide (neg up) norm
             , leftSide  = getSide (norm `cross` up) up
             , rightSide = getSide (up `cross` norm) up
             , gridPosition = (r*sideSize,c*sideSize)
             , localMap = lm
             }
      getSide nml expectUp = (idx, relRot expectUp actualUp rotAxis)
        where
          idx = fromJust $ findIndex (\(n,_)-> n==nml) faceDirs
          (rotAxis,actualUp) = faceDirs !! idx

  in zipWith makeSide faceDirs sidesRaw



data CubeWalkState = CubeWalkState { sides   :: [Side]
                                   , currentSide :: Int
                                   , posOnSide  :: Coord
                                   , dirOnSide :: Int
                                   , trail :: [(Int,Coord,Int)]
                                   , cwGridMap :: [String]
                                   }

instance Show CubeWalkState
  where show s = "currentSide = " ++ show (currentSide s) ++ "\n" ++
                 "pos on side = " ++ show (posOnSide s) ++ "\n" ++
                 "direction   = " ++ show (dirOnSide s) ++ "\n" ++
                 "actual Pos  = " ++ show (actualPos s)  ++ "\n" ++
                 unlines tehMap
          where
            tehMap = foldl addToMap (cwGridMap s) (trail s ++ [(currentSide s, posOnSide s, dirOnSide s)])
            addToMap g (side,pos,dir)  = zipWith (\row r-> zipWith (renderPos r) row [0..]) g [0..]
              where
                renderPos r v c = if (r,c) == getActualPos s side pos  then
                                    case dir of
                                      0 -> '>'
                                      1 -> 'v'
                                      2 -> '<'
                                      _ -> '^'
                                  else v


getActualPos :: CubeWalkState -> Int -> Coord -> Coord
getActualPos ss si pos = gridPosition (sides ss !! si) `cAdd` pos

actualPos :: CubeWalkState -> Coord
actualPos ss = getActualPos ss (currentSide ss) (posOnSide ss)

cubeAt :: CubeWalkState -> (Int,Coord) -> Char
cubeAt s (side,(r,c)) = localMap (sides s !! side) !! r !! c

isOffSide :: Side -> Coord -> Bool
isOffSide sd (r,c) = r<0 || r>=length lm || c<0 || c>=length (head lm)
  where lm = localMap sd

rotPos :: Int -> Int -> Coord -> Coord
rotPos _  0 p     = p
rotPos ss 1 (r,c) = (c,      ss-1-r)
rotPos ss 2 (r,c) = (ss-1-r, ss-1-c)
rotPos ss 3 (r,c) = (ss-1-c, r)
rotPos _ _ _ = error "blahasdfkasdf"

wrapCubePos :: Side -> Coord -> Int -> (Int,Coord,Int)
wrapCubePos sd (tr,tc) dir =
  let (newSide, rot) =
        case dir of
          0 -> rightSide sd
          1 -> downSide sd
          2 -> leftSide sd
          -- 3 -> upSide sd
          _ -> upSide sd
      sideSize = length (localMap sd)
      newPos = rotPos sideSize rot (tr `mod` sideSize, tc `mod` sideSize)
  in (newSide, newPos, (dir+rot) `mod` 4)

takeCubeStep :: CubeWalkState -> CubeWalkState
takeCubeStep s =
  if s`cubeAt`(moveSide,movePos) == '.' then s { currentSide = moveSide
                                               , posOnSide = movePos
                                               , dirOnSide = moveDir
                                               , trail = trail s ++ [trE]
                                               }
  else s
  where
    dir = dirOnSide s
    sideIdx = currentSide s
    side = sides s !! sideIdx
    (r,c) = posOnSide s
    tgtPos = case dir of
               0 -> (r,c+1)
               1 -> (r+1,c)
               2 -> (r,c-1)
               _ -> (r-1,c)
    trE@(moveSide, movePos, moveDir) = if isOffSide side tgtPos then wrapCubePos side tgtPos dir else (sideIdx,tgtPos,dir)

doCubeAction :: CubeWalkState -> Action -> CubeWalkState
doCubeAction s TurnR        = s { dirOnSide = (dirOnSide s + 1) `mod` 4 }
doCubeAction s TurnL        = s { dirOnSide = (dirOnSide s - 1) `mod` 4 }
doCubeAction s (Move steps) = (!!steps) $ iterate takeCubeStep s

part2 :: String -> Int
part2 contents =
  computePassword (actualPos finalState) (dirOnSide finalState)
  where
    finalState = foldl doCubeAction state0 actions
    state0 = CubeWalkState { sides = toCubeGrid grid
                           , currentSide = 0
                           , posOnSide  = (0,0)
                           , dirOnSide = 0
                           , trail = []
                           , cwGridMap  = grid
                           }
    (grid,actions) = parseStuff contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
