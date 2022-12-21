import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.Maybe
import qualified Data.Map as Map

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific

data Operation = Add | Sub | Mult | Div
  deriving(Show)

parseOp s = case s of
              "+" -> Add
              "-" -> Sub
              "*" -> Mult
              "/" -> Div
              _ -> error $ "Invalid operation string: "++s

doOp :: Operation -> Int -> Int -> Int
doOp Add v1 v2 = v1+v2
doOp Sub v1 v2 = v1-v2
doOp Mult v1 v2 = v1*v2
doOp Div v1 v2 = v1 `div` v2

data Expression = Value Int | BinOp Operation String String
  deriving(Show)

parseExpression :: [String] -> Expression
parseExpression [valStr] = Value (read valStr)
parseExpression [var1,opStr,var2] = BinOp (parseOp opStr) var1 var2
parseExpression _ = error "Empty expression"

parseLine l =
  let [name, exprStr] = splitOn ": " l
  in (name, parseExpression (words exprStr))

type ExprMap =  Map.Map String Expression

evalExpr :: Expression -> ExprMap -> Int
evalExpr (Value v) _= v
evalExpr (BinOp op var1 var2) exprMap =
  let exp1 = exprMap `at` var1
      exp2 = exprMap `at` var2
      val1 = evalExpr exp1 exprMap
      val2 = evalExpr exp2 exprMap
  in doOp op val1 val2

at :: ExprMap -> String -> Expression
at exprMap name = fromJust $ Map.lookup name exprMap

evalName :: String -> ExprMap -> Int
evalName name exprMap =
  evalExpr (exprMap `at` name) exprMap

-- part1 :: String -> Int
part1 contents =
  evalName "root" exprMap
  where
    exprMap = Map.fromList $ map parseLine $ lines contents

part2 :: String -> Int
part2 contents = length contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
