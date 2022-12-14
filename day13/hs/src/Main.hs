import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import Data.Char
import Data.List (elemIndex, sort)

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

data Item = Num Int | Lst [Item]
  deriving(Show,Eq)

type Signal = Item

instance Ord Item where
  (Num n1) `compare` (Num n2) = n1 `compare` n2
  (Lst lst1) `compare` (Lst lst2) = compareLists lst1 lst2
  i1@(Num _) `compare` i2@(Lst _)  = Lst [i1] `compare` i2
  i1@(Lst _)  `compare` i2@(Num _) = i1 `compare` Lst [i2]

compareLists :: [Item] -> [Item] -> Ordering
compareLists [] [] = EQ
compareLists [] _ = LT
compareLists _ [] = GT
compareLists (x:xs) (y:ys) =
  case x `compare` y of
    LT -> LT
    GT -> GT
    EQ -> compareLists xs ys

parseLst :: String -> ([Item],String)
parseLst s@(']':_) = ([],s)
parseLst s =
  let (it, ss) = parseItem s
  in case ss of
       (',':rest) -> (it:lst,sss)
         where (lst,sss) = parseLst rest
       _ -> ([it],ss)

parseItem :: String -> (Item,String)
parseItem ('[':s) =
  if head ss == ']' then (Lst lst, tail ss)
  else error "Invalid list item"
  where
    (lst,ss) = parseLst s
parseItem s = (Num (read istr),rest)
  where
    (istr,rest) = span isDigit s


parseSignal :: String -> Signal
parseSignal s =
  case it of
    Lst _ -> it
    _ -> error "incorrect signal format"
  where (it,_) = parseItem s

-- parse :: String -> [(Signal,Signal)]
parse = map (tuplify2 . map parseSignal . lines) . splitOn "\n\n"
  where
    tuplify2 [x,y] = (x,y)
    tuplify2 _ = error "not valid for tuplification"

part1 contents =
  let stuff = parse contents
  in sum [ i |(i,(s1,s2)) <- zip [1..] stuff, s1<s2]

part2 contents =
  let stuff = parse contents
      biglist = concatMap pairToList stuff
      pairToList (x,y) = [x,y]
      dp1 = Lst [Lst [Num 2]]
      dp2 = Lst [Lst [Num 6]]
      sorted = sort (dp1 : dp2 : biglist)
      (Just dp1i) = elemIndex dp1 sorted
      (Just dp2i) = elemIndex dp2 sorted
  in (dp1i+1)*(dp2i+1)


main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
