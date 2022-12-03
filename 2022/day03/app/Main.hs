{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Environment ( getArgs )
import Data.List.Split ( chunksOf )
import Data.List (intersect, nub)
import Data.Char ( ord, isLower )

splitLine :: String -> [String]
splitLine ln = chunksOf (length ln `div` 2) ln

getCharValue :: Char -> Int
getCharValue ch
    | isLower ch = ord ch - ord 'a' + 1
    | otherwise  = ord ch - ord 'A' + 27

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContents <- lines <$> readFile fileName

    let part1Input = map splitLine fileContents
    let part1CommonChars = concatMap (nub . foldr1 intersect) part1Input
    let part1Result = sum $ map getCharValue part1CommonChars

    let part2Input = chunksOf 3 fileContents
    let part2CommonChars = concatMap (nub . foldr1 intersect) part2Input
    let part2Result = sum $ map getCharValue part2CommonChars

    print part2Result
