import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

data Instr = Noop | Add Int
  deriving(Show)

parseInstructions =
  map parseInstruction . lines
  where
    parseInstruction line =
      case instr of
        "addx" -> Add (read $ head args)
        _ -> Noop
      where
        (instr:args) = words line

data CpuState = CpuState Int Int [Int]
  deriving(Show)

processSignalList (CpuState clk x ls) =
  CpuState clk x (if rem (clk-20) 40 == 0 then (clk*x):ls else ls)

doCpuNoop (CpuState clk x ls) = CpuState (clk+1) x ls
doCpuAddC1 = doCpuNoop
doCpuAddC2 i (CpuState clk x ls) = CpuState (clk+1) (x+i) ls

processCpuProgram =
  foldl execInstr state0
  where
    execInstr s Noop = processSignalList $ doCpuNoop s
    execInstr s (Add i)  = processSignalList $ doCpuAddC2 i $ processSignalList $ doCpuAddC1 s
    state0 = CpuState 1 1 []

-- part1 :: String -> Int
part1 contents =
  let program = parseInstructions contents
      (CpuState _ _ signal) = processCpuProgram program
  in sum signal

data CrtState = CrtState Int Int [[Char]]
  deriving(Show)

addCharToCrt newChar x crt =
  oldRows++[newLastRow]
  where
    updatedRows = if x == 0 then crt++[[]] else crt
    newLastRow = last updatedRows ++ [newChar]
    oldRows = init updatedRows

processCrtScreen (CrtState clk x crt) =
  CrtState clk x newCrt
  where
    xcoord = rem (clk-1) 40
    newChar = if abs (xcoord-x) <= 1 then '#' else '.'
    newCrt = addCharToCrt newChar xcoord crt

doCrtNoop (CrtState clk x crt) = CrtState (clk+1) x crt

doCrtAddC1 = doCrtNoop

doCrtAddC2 i (CrtState clk x crt) = CrtState (clk+1) (x+i) crt

processCrtProgram =
  foldl execInstr state0
  where
    execInstr s Noop = doCrtNoop $ processCrtScreen s
    execInstr s (Add i)  = doCrtAddC2 i $ processCrtScreen $ doCrtAddC1 $ processCrtScreen s
    state0 = CrtState 1 1 []

part2 :: String -> String
part2 contents =
  let program = parseInstructions contents
      (CrtState _ _ screen) = processCrtProgram program
  in unlines screen

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ part2answer
