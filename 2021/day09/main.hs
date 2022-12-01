import System.Environment
import Data.Char ( isAsciiLower, digitToInt )
import Data.Map ( (!), Map, fromList, toList, filterWithKey)
import Data.List ( sort, groupBy )
import Data.Set (Set, singleton, unions, empty, insert, notMember, member)

type Index = (Int, Int)
type Graph = Map Index (Int, [Index])

inRange :: (Ord a, Num a) => a -> a -> Bool
inRange bound value = 0 <= value && value < bound

getNeighbours :: Int -> Int -> Index -> [Index]
getNeighbours numCols numRows (x, y) =
    [(x', y) | x' <- xs, (x', y) /= (x, y)] ++ [(x, y') | y' <- ys, (x, y') /= (x, y)]
    where xs = filter (inRange numRows) [x-1, x, x+1]
          ys = filter (inRange numCols) [y-1, y, y+1]

parseInput :: [[Int]] -> Graph
parseInput parsedContents = fromList $ zipWith toEdge heights indices
    where numCols = (length . head) parsedContents
          numRows = length parsedContents
          heights = concat parsedContents
          indices = [(x, y) | x <- [0..numRows-1], y <- [0..numCols-1]]
          getNeighbours' = getNeighbours numCols numRows
          toEdge height index = (index, (height, getNeighbours' index))

isLowPoint :: Graph -> Index -> Bool
isLowPoint g index = targetHeight < minimum neighbouringHeights
    where targetHeight = fst (g ! index)
          neighbouringHeights = map (fst . (g !)) (snd (g ! index))

getLowPoints :: Graph -> Graph
getLowPoints g = filterWithKey (\index _ -> isLowPoint g index) g

neighbourPartOfBasin :: Graph -> Index -> Set Index -> Index -> Bool
neighbourPartOfBasin _ _ visitedPoints neighbour | neighbour `member` visitedPoints = False
neighbourPartOfBasin g _ _ neighbour | fst (g!neighbour) == 9 = False
neighbourPartOfBasin g point _ neighbour = fst (g!point) <= fst (g!neighbour)

getBasin :: Graph -> Set Index -> Index -> Set Index
getBasin g visitedPoints point = unions (singleton point: map (getBasin g newVisitedPoints) neighbours)
    where (ownHeight, ownNeighbours) = g!point
          neighbours = filter (neighbourPartOfBasin g point visitedPoints) ownNeighbours
          newVisitedPoints = point `insert` visitedPoints

heightsFromGraph :: Graph -> [Int]
heightsFromGraph = map (fst . snd) . toList

indicesFromGraph :: Graph -> [Index]
indicesFromGraph = map fst . toList

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = map (map digitToInt) $ words contents
    let graph = parseInput parsedContents
    let lowPoints = indicesFromGraph $ getLowPoints graph
    let basins = map (length . getBasin graph empty) lowPoints
    let largestThreeBasins = take 3 $ (reverse . sort) basins
    print $ product largestThreeBasins
