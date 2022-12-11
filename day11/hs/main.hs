import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List.Split
import Data.List (sort)

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff
data Op = Square | Add Int | Mult Int
  deriving(Show)

applyOp Square i = i*i
applyOp (Add t) i = i+t
applyOp (Mult f) i = i*f

data Monkey = Monkey [Int] Op Int Int Int
  deriving(Show)

data MonkeyWithCount = MonkeyWithCount Monkey Int
  deriving(Show)


parseMonkey :: String -> Monkey
parseMonkey s =
  Monkey items op divBy trueDst falseDst
  where
    ls = lines s
    items = map read $ splitOn ", " $ (!!1) $ splitOn ": " $ (!!1) ls
    optokens = words $ (!!1) $ splitOn "new = " $ (!!2) ls
    op = case (optokens !! 1, optokens !! 2) of
      ("+",o2) -> Add $ read o2
      ("*","old") -> Square
      ("*",o2) -> Mult $ read o2
    divBy = read $ last $ words $ (!!3) ls
    trueDst = read $ last $ words $ (!!4) ls
    falseDst = read $ last $ words $ (!!5) ls

withoutItems (MonkeyWithCount (Monkey items op d t f) cnt) = MonkeyWithCount (Monkey [] op d t f) (cnt+length items)
removeTurnItems i ms =
  zipWith replaceMonkey ms [0..]
  where
    replaceMonkey m ii = if i==ii then withoutItems m else m

addItemAtIndex item dst m@(MonkeyWithCount (Monkey items op d t f) cnt) i =
  if i/=dst then m
  else MonkeyWithCount (Monkey (items++[item]) op d t f) cnt

monkeyTurn wlOp ms i =
  foldl processItem withoutItems items
  where
    (MonkeyWithCount (Monkey items op divBy trueDst falseDst) count) = ms !! i
    withoutItems = removeTurnItems i ms
    processItem woi it = zipWith addItem woi [0..]
      where
        addItem = addItemAtIndex newWl dst
        newWl = wlOp (applyOp op it)
        dst = if rem newWl divBy == 0 then trueDst else falseDst

doRound wlOp monkeys =
  foldl (monkeyTurn wlOp) monkeys $ take (length monkeys) [0..]

business ms = product $ take 2 $ reverse $ sort $ map (\(MonkeyWithCount _ cnt) -> cnt) ms

-- part1 :: String -> Int
part1 contents =
  business finalMonkeys
  where
    finalMonkeys = (!!20) $ iterate (doRound (`div` 3)) $ map (\m -> (MonkeyWithCount m 0)) monkeys
    monkeys = map parseMonkey $ splitOn "\n\n" contents

-- part2 :: String -> Int
part2 contents =
  business finalMonkeys
  where
    finalMonkeys = (!!10000) $ iterate (doRound (`rem` divProd)) $ map (\m -> (MonkeyWithCount m 0)) monkeys
    divProd = product $ map (\(Monkey _ _ d _ _) -> d) monkeys
    monkeys = map parseMonkey $ splitOn "\n\n" contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
