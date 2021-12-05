import System.Environment
import Data.List
import Data.Ord
import Debug.Trace
import Data.List.Split

data Vector = Vector {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving (Show, Eq)

inLine :: Vector -> Int -> Int -> Bool
inLine (Vector x1 y1 x2 y2) x y
    | x1 == x2 = x == x1
    | otherwise = y == m*x + b
    where m = (y1 - y2) `div` (x1 - x2)
          b = y1 - m*x1

parseLine :: String -> Vector
parseLine line = Vector x1 y1 x2 y2
    where tokens = words line
          str1 = head tokens
          str2 = last tokens
          [x1, y1] = map read $ splitOn "," str1 :: [Int]
          [x2, y2] = map read $ splitOn "," str2 :: [Int]

generatePoints :: Vector -> [(Int, Int)]
generatePoints points@(Vector x1 y1 x2 y2)
    | x1 == x2 = [(x1, y2 + signY*d) | d <- [0 .. deltaY]]
    | y1 == y2 = [(x2 + signX*d, y1) | d <- [0 .. deltaX]]
    | otherwise = [(x2 + signX*d, y2 + signY*d) | d <- [0 .. deltaX]]
    where deltaX = abs (x1 - x2)
          deltaY = abs (y1 - y2)
          signX = if x1 - x2 < 0 then -1 else 1
          signY = if y1 - y2 < 0 then -1 else 1

calculateScore :: [(Int, Int)] -> Int
calculateScore points = length duplicates
    where duplicates = (filter (\x -> length x >= 2) . group . sort) points

isDiagonal :: Vector -> Bool
isDiagonal (Vector x1 y1 x2 y2)
    | x1 == x2 = True 
    | y1 == y2 = True
    | otherwise = False

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let fileLines = lines contents

    let vectors = map parseLine fileLines
    
    let allPoints = concatMap generatePoints vectors

    print $ calculateScore allPoints
