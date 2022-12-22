import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.Maybe
import Text.Read
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

invOp :: Operation -> Operation
invOp Add = Sub
invOp Sub = Add
invOp Mult = Div
invOp Div = Mult

data Expression = Value Int | BinOp Operation Expression Expression | Var String
  deriving(Show)

type ExprMap =  Map.Map String [String]

parseExpression :: ExprMap -> [String] -> Expression
parseExpression _ [str] = case readMaybe str of
                            Just val -> Value val
                            Nothing -> Var str
parseExpression expMap [var1,opStr,var2] = BinOp (parseOp opStr) expr1 expr2
  where expr1 = parseExpression expMap (expMap `at` var1)
        expr2 = parseExpression expMap (expMap `at` var2)
parseExpression _ _ = error "Empty expression"

parseLine l =
  let [name, exprStr] = splitOn ": " l
  in (name, words exprStr)

simplify :: Expression -> Expression
simplify (BinOp op e1 e2) =
  let s1 = simplify e1
      s2 = simplify e2
  in case (s1,s2) of
       (Value v1,Value v2) -> Value (doOp op v1 v2)
       _ -> BinOp op s1 s2
simplify e@(Var _) = e
simplify e = e

at :: ExprMap -> String -> [String]
at exprMap name = fromJust $ Map.lookup name exprMap


part1 :: String -> Int
part1 contents =
  val
  where
    (Value val) = simplify rootExpr
    rootExpr = parseExpression exprMap (exprMap `at` "root")
    exprMap = Map.fromList $ map parseLine $ lines contents

reroot :: Expression -> String -> Expression -> Expression
reroot (Var name) tgtVar rootTree = if name == tgtVar then rootTree else error "Invalid variable"
reroot (BinOp Add e1 e2@(Value _)) tgtVar rootTree = reroot e1 tgtVar (BinOp Sub rootTree e2)
reroot (BinOp Add e1@(Value _) e2) tgtVar rootTree = reroot e2 tgtVar (BinOp Sub rootTree e1)
reroot (BinOp Sub e1 e2@(Value _)) tgtVar rootTree = reroot e1 tgtVar (BinOp Add rootTree e2)
reroot (BinOp Sub e1@(Value _) e2) tgtVar rootTree = reroot e2 tgtVar (BinOp Sub e1 rootTree)
reroot (BinOp Mult e1 e2@(Value _)) tgtVar rootTree = reroot e1 tgtVar (BinOp Div rootTree e2)
reroot (BinOp Mult e1@(Value _) e2) tgtVar rootTree = reroot e2 tgtVar (BinOp Div rootTree e1)
reroot (BinOp Div e1 e2@(Value _)) tgtVar rootTree = reroot e1 tgtVar (BinOp Mult rootTree e2)
reroot (BinOp Div e1@(Value _) e2) tgtVar rootTree = reroot e2 tgtVar (BinOp Div e1 rootTree)
reroot _ _ _ = error "Invalid expression"


part2 :: String -> Int
part2 contents =
  val
  where
    (Value val) = simplify newTree
    newTree = reroot h00manTree "h00man" valTree
    (h00manTree, valTree) = case (simplify leftTree, simplify rightTree) of
                              (v@(Value _), ht) -> (ht,v)
                              (ht,v@(Value _)) -> (ht,v)
                              _ -> error "Bad stuff"

    leftTree = parseExpression exprMap (exprMap `at` leftTreeName)
    rightTree = parseExpression exprMap (exprMap `at` rightTreeName)
    [leftTreeName,_,rightTreeName] = exprMapRaw `at` "root"
    exprMap = Map.insert "humn" ["h00man"] exprMapRaw
    exprMapRaw = Map.fromList $ map parseLine $ lines contents

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer:\n" ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer:\n" ++ show part2answer
