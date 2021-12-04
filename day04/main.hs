import System.Environment
import Data.List
import Data.Char ( digitToInt )
import Data.Ord
import Debug.Trace
import Data.List.Split

type Board = [[Int]]

readBoard :: [String] -> Board
readBoard rawBoard = rows ++ columns
    where rows = map (map read . words) $ tail $ take 6 rawBoard
          columns = transpose rows

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards input = readBoard rawBoard : parseBoards (drop 6 input)
    where rawBoard = take 6 input

isSubset :: [Int] -> [Int] -> Bool
isSubset l2 = all (`elem` l2)

turnsForBingo :: [Int] -> Board -> Int
turnsForBingo input board
    | hasBingo = turnsForBingo (init input) board
    | otherwise = 1 + length input
    where hasBingo = any (isSubset input) board

bestBoard :: [Board] -> [Int] -> Board
bestBoard boards input = minimumBy (comparing (turnsForBingo input)) boards

worstBoard :: [Board] -> [Int] -> Board
worstBoard boards input = maximumBy (comparing (turnsForBingo input)) boards

getBoardScore :: Board -> [Int] -> Int
getBoardScore board input = sum elementsNot * last input
    where uniqueElements = (nub . concat) board
          (_, elementsNot) = partition (`elem` input) uniqueElements

part1 :: [Board] -> [Int] -> IO ()
part1 parsedBoards bingoDraws = do
    let winningBoard = bestBoard parsedBoards bingoDraws
    let winningTurns = turnsForBingo bingoDraws winningBoard
    let winningInput = take winningTurns bingoDraws
    print $ getBoardScore winningBoard winningInput

part2 :: [Board] -> [Int] -> IO ()
part2 parsedBoards bingoDraws = do
    let winningBoard = worstBoard parsedBoards bingoDraws
    let winningTurns = turnsForBingo bingoDraws winningBoard
    let winningInput = take winningTurns bingoDraws
    print $ getBoardScore winningBoard winningInput

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let fileLines = lines contents

    let input = splitOn "," (head fileLines)

    let bingoDraws = map read input :: [Int]

    let parsedBoards = parseBoards $ tail fileLines :: [Board]

    part1 parsedBoards bingoDraws
    part2 parsedBoards bingoDraws
