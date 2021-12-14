import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.Map (Map, fromList, (!), member)
import Data.List (sort, group, concatMap)
import Data.MultiSet (MultiSet, concatMap, fromList, toList, elems, distinctSize, toSet, occur, toOccurList)

type InsertionRuleSet = Map (Char, Char) Char

window2 :: Ord a => [a] -> [(a, a)]
window2 originalList@(a:b:_) = (a, b) : window2 (tail originalList)
window2 _ = []

parseInsertionRule :: String -> ((Char, Char), Char)
parseInsertionRule line = ((c1, c2), r)
    where [[c1, c2], [r]] = splitOn " -> " line

applyInsertionRule :: InsertionRuleSet -> (Char, Char) -> [(Char, Char)]
applyInsertionRule insertionRules pair@(c1, c2) 
    | pair `member` insertionRules = [(c1, newItem), (newItem, c2)]
    | otherwise = [pair]
    where newItem = insertionRules ! pair

getNextStep :: InsertionRuleSet -> MultiSet (Char, Char) -> MultiSet (Char, Char)
getNextStep insertionRules = Data.MultiSet.concatMap (applyInsertionRule insertionRules)

getOccurrences :: MultiSet (Char, Char) -> [(Char, Int)]
getOccurrences = Data.MultiSet.toOccurList . Data.MultiSet.concatMap (\(c1, c2) -> [c1, c2])

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let ([template], insertionRules) = splitAt 1 (lines contents)
    let initialStep = Data.MultiSet.fromList $ window2 template
    let parsedInsertionRules = Data.Map.fromList $ map parseInsertionRule (filter (not . null) insertionRules)

    let iterations = iterate (getNextStep parsedInsertionRules) initialStep
    let result = sort . map snd $ getOccurrences (iterations !! 40)
    print $ (last result - head result + 1) `div` 2
