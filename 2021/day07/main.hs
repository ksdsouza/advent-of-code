import System.Environment
import Data.List
import Data.List.Split

constantRateCost :: Int -> Int -> Int
constantRateCost x = abs . (x -)

linearRateCost :: Int -> Int -> Int
linearRateCost x y = n * (n+1) `div` 2
    where n = abs (x - y)

calculateCost :: (Int -> Int -> Int) -> [Int] -> Int -> Int
calculateCost costFn initialPositions newPosition = sum $ map (costFn newPosition) initialPositions

median :: [Int] -> Int
median nums = sort nums !! (length nums `div` 2)

mean :: [Int] -> Int
mean nums = sum nums `div` length nums

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = map read $ splitOn "," contents :: [Int]
    let largestValue = maximum parsedContents

    print $ (minimum . map (calculateCost constantRateCost parsedContents)) [0..largestValue]
    print $ (minimum . map (calculateCost linearRateCost parsedContents)) [0..largestValue]

    -- Taking the median for pt1/mean for pt2 is the optimized version of the bruteforce approach above
    -- print $ calculateCost constantRateCost parsedContents (median parsedContents)
    -- print $ calculateCost linearRateCost parsedContents (mean parsedContents)
