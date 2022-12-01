import System.Environment
import Data.Char (digitToInt)
import Graph.DijkstraSimple
    ( findPath, EdgeTo(EdgeTo), Graph(Graph), Path(pathWeight) )
import Data.List.NonEmpty (fromList)
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Graph.DijkstraSimple.Weighters (cumulativeWeighter)

type Node = (Int, Int)
type Weight = Int
type Edge = EdgeTo Node Weight

indices :: Int -> [Node]
indices numRows = [(x, y) | x<-[0..numRows-1], y <- [0..numRows-1]]

inRange :: (Ord a, Num a) => a -> a -> Bool
inRange limit x = 0 <= x && x < limit

neighbours :: Int -> Node -> [Node]
neighbours numRows (x, y) = [(x, y') | y' <- ys, (x, y') /= (x, y)] ++ [(x', y) | x' <- xs, (x', y) /= (x, y)]
    where xs = filter (inRange numRows) [x-1, x, x+1]
          ys = filter (inRange numRows) [y-1, y, y+1]

getEdges :: Int -> Map Node Weight -> Node -> (Node, [Edge])
getEdges numRows indexToWeight node = (node, outgoingEdges)
    where ns = neighbours numRows node
          outgoingEdges = map (\pt -> EdgeTo pt (indexToWeight ! pt)) ns

shiftWeight :: Int -> Weight -> Weight
shiftWeight offset w | w + offset >= 10 = w + offset - 9
shiftWeight offset w = w + offset

getShiftedPoints :: Int -> (Node, Weight) -> [(Node, Weight)]
getShiftedPoints numRows ((x, y), w) = [
        ((x + xOffset, y + yOffset), _getOffsetWeight xOffset yOffset w) | xOffset <- offsets, yOffset <- offsets
    ]
    where offsets = map (numRows*) [0..4]
          _getOffsetWeight xOffset yOffset w = shiftWeight (quot xOffset numRows + quot yOffset numRows) w

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let contentLines = words contents
    let numRows = length contentLines
    let weights = concatMap (map digitToInt) $ words contents

    let nodes = indices numRows

    let fullNumRows = 5 * numRows
    let fullNodes = indices fullNumRows

    let indexToWeight = Map.fromList $ concatMap (getShiftedPoints numRows) $ zip nodes weights
    let graph = Graph $ Map.fromList (map (getEdges fullNumRows indexToWeight) fullNodes)
    let (Just shortestPath) = findPath graph (head fullNodes) cumulativeWeighter (last fullNodes)
    print $ pathWeight shortestPath
