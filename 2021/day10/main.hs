import System.Environment
import Data.List (sort)

data Status = COMPLETE | INCOMPLETE {remainingOpen :: String } | CORRUPTED {corruptedValue :: Char} deriving (Show, Eq)

isClosedChar :: Char -> Bool
isClosedChar = (`elem` [']', '}', '>', ')'])

samePair :: Char -> Char -> Bool
samePair '[' ']' = True
samePair '<' '>' = True
samePair '{' '}' = True
samePair '(' ')' = True
samePair _ _ = False

scoreChar :: Char -> Int
scoreChar ')' = 3
scoreChar ']' = 57
scoreChar '}' = 1197
scoreChar '>' = 25137
scoreChar c = error ("Invalid character for scoring " ++ [c])

incompeteScoreChar :: Char -> Int
incompeteScoreChar ')' = 1
incompeteScoreChar ']' = 2
incompeteScoreChar '}' = 3
incompeteScoreChar '>' = 4
incompeteScoreChar c = error ("Invalid character for scoring " ++ [c])

checkLine :: [Char] -> String -> Status
checkLine [] [] = COMPLETE
checkLine stack [] = INCOMPLETE stack
checkLine [] [c] | isClosedChar c = CORRUPTED c
checkLine stack (c1:rest) | not (isClosedChar c1) = checkLine (c1:stack) rest
checkLine (c:restStack) (c1:restStr) | samePair c c1 = checkLine restStack restStr
checkLine stack str = CORRUPTED (head str)

isCorrupted :: Status -> Bool
isCorrupted (CORRUPTED _) = True
isCorrupted _ = False

isIncomplete :: Status -> Bool
isIncomplete (INCOMPLETE _) = True
isIncomplete _ = False

getMatchingPair :: Char -> Char
getMatchingPair '[' = ']'
getMatchingPair '(' = ')'
getMatchingPair '<' = '>'
getMatchingPair '{' = '}'
getMatchingPair c = error ("Invalid char " ++ [c])

scoreIncompleteLine :: String -> Int
scoreIncompleteLine line = foldl (\acc score -> (acc * 5) + score) 0 characterScores
    where characterScores = map (incompeteScoreChar . getMatchingPair) line

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContent = words contents
    let syntaxChecks = map (checkLine []) parsedContent
    
    -- Part 1
    let corruptedLines = filter isCorrupted syntaxChecks
    print $ sum $ map (scoreChar . corruptedValue) corruptedLines

    -- Part 2
    let incompleteLines = filter isIncomplete syntaxChecks
    let incompleteScores = map (scoreIncompleteLine . remainingOpen) incompleteLines
    print $ sort incompleteScores !! (length incompleteScores `div` 2)
    