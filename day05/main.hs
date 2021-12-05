import System.Environment
import Data.List
import Data.Ord
import Debug.Trace
import Data.List.Split

data Vector = Vector {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving (Show, Eq)

parseLine :: String -> Vector
parseLine line = Vector x1 y1 x2 y2
    where tokens = words line
          (point1, point2) = (head tokens, last tokens)
          [x1, y1] = map read $ splitOn "," point1
          [x2, y2] = map read $ splitOn "," point2

generatePoints :: Vector -> [(Int, Int)]
generatePoints (Vector x1 y1 x2 y2)
    | x1 == x2 = [(x1, y2 + signY*d) | d <- [0 .. deltaY]]
    | y1 == y2 = [(x2 + signX*d, y1) | d <- [0 .. deltaX]]
    | otherwise = [(x2 + signX*d, y2 + signY*d) | d <- [0 .. deltaX]]
    where deltaX = abs (x1 - x2)
          deltaY = abs (y1 - y2)
          signX = signum (x1 - x2)
          signY = signum (y1 - y2)

overlappingPoints :: [(Int, Int)] -> [(Int, Int)]
overlappingPoints = map head . filter (\x -> length x >= 2) . group . sort

calculateScore :: [(Int, Int)] -> Int
calculateScore = length . overlappingPoints

isDiagonal :: Vector -> Bool
isDiagonal (Vector x1 y1 x2 y2) = not (x1 == x2 || y1 == y2)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let fileLines = lines contents

    let vectors = map parseLine fileLines
    let part1Vectors = filter (not . isDiagonal) vectors
    let allPoints = concatMap generatePoints vectors

    print $ calculateScore allPoints
