import System.Environment
import Data.Char ( isAsciiLower )
import Data.Map ( (!), Map, fromList )
import Data.List.Split ( splitOn )
import Data.List ( sort, groupBy )
import Data.MultiSet (MultiSet, insert, singleton, distinctSize, size, empty)

type Graph = Map String [String]

isSmallCave :: String -> Bool
isSmallCave = all isAsciiLower

traverse' :: Graph -> String -> MultiSet String -> String -> Int
traverse' _ _ _ "start" = 0
traverse' _ _ _ "end" = 1
traverse' g currentCave visitedCaves nextCave
    | isSmallCave nextCave && nextCave `elem` visitedCaves && usedSmallCaveReturnRule = 0
    | isSmallCave nextCave = caveTraverse g nextCave (insert nextCave visitedCaves)
    | otherwise = caveTraverse g nextCave visitedCaves
    where usedSmallCaveReturnRule = size visitedCaves /= distinctSize visitedCaves

caveTraverse :: Graph -> String -> MultiSet String -> Int
caveTraverse g currentCave visitedSmallCaves = sum $ map (traverse' g currentCave visitedSmallCaves) neighbours
    where neighbours = g ! currentCave

parseCaveTunnel :: String -> [(String, String)]
parseCaveTunnel input = [(cave1, cave2), (cave2, cave1)]
    where [cave1, cave2] = splitOn "-" input

groupNeighbours :: [(String, String)] -> (String, [String])
groupNeighbours values@((c1, _):_) = (c1, map snd values)
groupNeighbours _ = error "Bad!"

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = map groupNeighbours $ groupBy (\(s1, _) (s2, _) -> s1 == s2) . sort $ concatMap parseCaveTunnel (lines contents)
    let caveGraph = fromList parsedContents

    print $ caveTraverse caveGraph "start" Data.MultiSet.empty
