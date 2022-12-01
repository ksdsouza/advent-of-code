import System.Environment
import Data.Map ( (!), adjust, toList, fromList, foldr, foldrWithKey, map, Map )
import Data.List
import Data.Char

type Energy = Int
type Flashes = Int
type Index = (Int, Int)
type Edge = (Energy, Flashes, [Index])
type Graph = Map Index Edge

increaseEnergy :: Index -> Graph -> Graph
increaseEnergy index g =
    case g ! index of
        (9, flashes, neighbours) -> Prelude.foldr increaseEnergy mapAfterFlashing neighbours
        _ -> mapAfterIncreasingOwnEnergy
    where mapAfterIncreasingOwnEnergy = adjust (\(e, f, n) -> (e+1, f, n)) index g
          mapAfterFlashing = adjust (\(e, f, n) -> (e+1, f+1, n)) index g

finalizeEnergy :: Edge -> Edge
finalizeEnergy (e, f, n) | e >= 10 = (0, f, n)
finalizeEnergy edge = edge

finalizeStep :: Graph -> Graph
finalizeStep = Data.Map.map finalizeEnergy

performStep :: Graph -> Graph
performStep g = finalizeStep $ foldrWithKey (\index _ -> increaseEnergy index) g g

numFlashes :: Graph -> Int
numFlashes = Data.Map.foldr (\(_, f, _) acc -> f + acc) 0

indices :: [Index]
indices = [(x, y) | x <- [0..9], y <- [0..9]]

neighbours :: Index -> [Index]
neighbours (x, y) = [(x', y') | x' <-xs , y' <- ys, (x', y') /= (x, y)]
    where xs = Prelude.filter inRange [x-1, x, x+1]
          ys = Prelude.filter inRange [y-1, y, y+1]
          inRange v = 0 <= v && v <= 9

trim = filter (/= '\n')

parseEdge :: (Char, Index) -> (Index, Edge)
parseEdge (initialEnergy, index) = (index, (digitToInt initialEnergy, 0, neighbours index))

parseGraph :: [(Char, Index)] -> Graph
parseGraph = fromList . Data.List.map parseEdge

nonConsecutiveFlash :: Graph -> Bool
nonConsecutiveFlash g = 0 /= Data.Map.foldr (\(e, _, _) acc -> acc+e) 0 g

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContent = zip (trim contents) indices
    let parsedGraph = parseGraph parsedContent

    -- part1
    -- print $ iterate performStep parsedGraph !! 100

    let steps = iterate performStep parsedGraph
    let stepNumsForSyncedFlashes = last . Data.List.map fst $ takeWhile (\(_, g) -> nonConsecutiveFlash g) (zip [1..] steps)
    print stepNumsForSyncedFlashes
    