import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Debug.Trace
import Data.Function

-- generic

getArgContents :: IO String
getArgContents = do
  args <- getArgs
  Control.Monad.when (null args) $ die "Insufficient arguments"
  let filename = head args
  handle <- openFile filename ReadMode
  hGetContents handle

-- problem specific stuff

getCommands contents =
  [(cmd item, output item )| item <- splitOn "$ " contents, item /= ""]
  where
    cmd = head . lines
    output = tail . lines

toAbsoluteDirListings :: [(String,[String])] -> [([String],[String])]
toAbsoluteDirListings commands =
  listings
  where
    (_, listings) = foldl processCommand ([], []) commands
    processCommand (path, listings) (cmd,output) =
      if cmd == "ls" then
        (path, listings++[(path,output)])
      else
        let [_,cdarg] = words cmd
        in case cdarg of
             "/" -> ([], listings)
             ".." -> (init path, listings)
             _ -> (path++[cdarg], listings)

data FileNode = File String Int | Dir String [FileNode] 

fileSize :: FileNode -> Int
fileSize (File _ sz) = sz
fileSize (Dir _ children) = sum $ map fileSize children

getDirSizes :: String -> FileNode -> [(String,Int)]
getDirSizes _ (File _ _) = []
getDirSizes prefix n@(Dir name children) =
  (myPath, fileSize n): concatMap (getDirSizes newPrefix) children
  where
    myPath = if null prefix then name else prefix++name
    newPrefix = if null prefix then name else prefix++name++"/"

showDir :: Int -> FileNode -> String
showDir level node =
  replicate level ' ' ++ case node of
                           File name size -> name ++ " " ++ show size ++ "\n"
                           Dir name contents -> name ++ "\n"++ concatMap (showDir (level+2)) contents
  
newtype Tree = Tree FileNode
instance Show Tree where
  show (Tree rootnode) = showDir 0 rootnode
 
buildTree :: [(String, [String])] -> Tree
buildTree commands =
  Tree (Dir "/"  $ foldl addListing [] listings)
  where
    listings = toAbsoluteDirListings commands
    -- addListing dir (path,items) = trace ("\naddListing "++show dir ++ " (" ++ show path++","++show items++") --> ") $ traceShowId $ if null path then
    addListing dir (path,items) = if null path then map convertItem items
                                  else insertItems (head path) (tail path) items dir
    convertItem item = if q == "dir" then Dir name [] else File name $ read q
      where [q, name] = words item
    insertItems dirname path items dir = map banan dir
      where banan n | (Dir name cont) <- n, name == dirname = Dir dirname $ addListing cont (path, items)
                    | otherwise = n
  
getTreeDirSizes :: Tree -> [(String, Int)]
getTreeDirSizes (Tree rootnode) =
  getDirSizes "" rootnode

  
part1 :: String -> Int
part1 contents =
  let dirSizes = getTreeDirSizes $ buildTree $ getCommands contents
  in
    sum $ [ sz | (name,sz) <- dirSizes, sz<= 100000]

-- part2 :: String -> Int
part2 contents = 
  let dirSizes = getTreeDirSizes $ buildTree $ getCommands contents
      sorted = map snd $ sortBy (compare `on` snd) dirSizes
      total = last sorted
      limit = 40000000
      toBeDeleted = total-limit
  in
    head $ dropWhile (<toBeDeleted) sorted

main :: IO ()
main = do
  contents <- getArgContents
  let part1answer = part1 contents
  putStrLn $ "Part1 answer: " ++ show part1answer
  let part2answer = part2 contents
  putStrLn $ "Part2 answer: " ++ show part2answer
