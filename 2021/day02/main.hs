import System.Environment
import Data.List

data Direction = Forward | Down | Up deriving (Show, Eq)
data Movement = Movement {direction :: Direction, magnitude :: Int} deriving Show

isVerticalMovement :: Movement -> Bool
isVerticalMovement (Movement Forward _) = False
isVerticalMovement _ = True

parseLine :: String -> Movement
parseLine line
        | direction == "forward" = Movement Forward (read magnitude)
        | direction == "up" = Movement Up (-read magnitude)
        | direction == "down" = Movement Down (read magnitude)
        | otherwise = error ("Invalid line " ++ line)
    where [direction, magnitude] = words line

applyMovement :: (Int, Int, Int) -> Movement -> (Int, Int, Int)
applyMovement (horizontalPosn, aim, depth) (Movement direction magnitude)
    | direction == Forward = (horizontalPosn + magnitude, aim, depth + aim * magnitude)
    | otherwise = (horizontalPosn, aim + magnitude, depth)

part1 :: [Movement] -> Int
part1 directions = verticalDistance * horizontalDistance
    where (verticalDirections, horizontalDirections) = partition isVerticalMovement directions
          verticalDistance = sum $ map magnitude verticalDirections
          horizontalDistance = sum $ map magnitude horizontalDirections

part2 :: [Movement] -> Int
part2 directions = finalHorizontalPosn * finalDepth
    where (finalHorizontalPosn, _, finalDepth) = foldl applyMovement (0, 0, 0) directions

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContent = (map parseLine . lines) contents
    -- print $ part1 parsedContent
    print $ part2 parsedContent
