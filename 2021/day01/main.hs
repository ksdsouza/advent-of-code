import System.Environment
import Data.List

maskAscendingDepths :: Ord a => [a] -> [Bool]
maskAscendingDepths depths = zipWith (>) (tail depths) depths

numDepthIncreases :: Ord a => [a] -> Int
numDepthIncreases = length . filter id . maskAscendingDepths

window3 :: Ord a => [a] -> [(a, a, a)]
window3 originalList@(a:b:c:_) = (a, b, c) : window3 (tail originalList)
window3 _ = []

sumWindow3 :: Num a => (a, a, a) -> a
sumWindow3 (x1, x2, x3) = x1 + x2 + x3

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (map read . lines) contents :: [Int]
    print . numDepthIncreases . map sumWindow3 . window3 $ parsedContents
