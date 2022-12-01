{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment ( getArgs )
import Data.List
import Data.Ord

import Data.Text ( Text, pack, splitOn, unpack )

parseInput :: Text -> [[Int]]
parseInput contents = map (map read) groupedLines
    where
        groupedLines :: [[String]] = map (lines . unpack) (splitOn "\n\n" contents)

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 = sum . take 3 . sortOn Down

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName

    let parsedContents = parseInput (pack contents)
    let totalCaloriesPerElf :: [Int] = map sum parsedContents
    print $ part1 totalCaloriesPerElf
    print $ part2 totalCaloriesPerElf
