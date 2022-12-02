{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Environment ( getArgs )

lose = 0
draw = 3
win = 6

pointsForAction :: String -> Int
pointsForAction [_, _, 'X'] = 1
pointsForAction [_, _, 'Y'] = 2
pointsForAction [_, _, 'Z'] = 3

needToLose :: Char -> Char
needToLose 'A' = 'Z'
needToLose 'B' = 'X'
needToLose 'C' = 'Y'

needToWin :: Char -> Char
needToWin 'A' = 'Y'
needToWin 'B' = 'Z'
needToWin 'C' = 'X'

needToDraw :: Char -> Char
needToDraw 'A' = 'X'
needToDraw 'B' = 'Y'
needToDraw 'C' = 'Z'

pointsForOutcome :: String -> Int
pointsForOutcome "A X" = draw
pointsForOutcome "A Y" = win
pointsForOutcome "A Z" = lose
pointsForOutcome "B X" = lose
pointsForOutcome "B Y" = draw
pointsForOutcome "B Z" = win
pointsForOutcome "C X" = win
pointsForOutcome "C Y" = lose
pointsForOutcome "C Z" = draw

convertPart2File :: [Char] -> [Char]
convertPart2File [opp, space, 'X'] = [opp, space, needToLose opp]
convertPart2File [opp, space, 'Y'] = [opp, space, needToDraw opp]
convertPart2File [opp, space, 'Z'] = [opp, space, needToWin opp]

main :: IO ()
main = do
    (fileName:_) <- getArgs
    rawFile :: [String] <- lines <$> readFile fileName
    let part2converted :: [String] = map convertPart2File rawFile
    let results :: Int = sum $ zipWith (+) (map pointsForOutcome part2converted) (map pointsForAction part2converted)

    print results
