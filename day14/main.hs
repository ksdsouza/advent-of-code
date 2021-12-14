import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.Map (Map, fromList, (!), member)
import Data.List (sort, group, concatMap)
import Data.MultiSet (MultiSet, concatMap, fromList, toList, elems, distinctSize, toSet, occur, toOccurList)

type InsertionRuleSet = Map (Char, Char) Char

window2 :: Ord a => [a] -> [(a, a)]
-- Groups adjacent elements of a list together
window2 originalList@(a:b:_) = (a, b) : window2 (tail originalList)
window2 _ = []

parseInsertionRule :: String -> ((Char, Char), Char)
parseInsertionRule line = ((c1, c2), r)
    where [[c1, c2], [r]] = splitOn " -> " line

applyInsertionRule :: InsertionRuleSet -> (Char, Char) -> [(Char, Char)]
applyInsertionRule insertionRules pair@(c1, c2) 
    -- if the pair (A, B) we're working on has an insertion rule for it (AB->C),
    -- add 2 element pairs for the next insertion (A, C), (C, B)
    -- (corresponding to producing "ACB" for this part of the polymer)
    | pair `member` insertionRules = [(c1, newItem), (newItem, c2)]
    -- no-op if there's no insertion rule defined for the pair. Include the pair
    -- for the next step so the overall counts make sense at the end
    | otherwise = [pair]
    where newItem = insertionRules ! pair

getNextStep :: InsertionRuleSet -> MultiSet (Char, Char) -> MultiSet (Char, Char)
-- Applies one step of the polymerization process.
-- getNextStep takes the insertion rules, and the multiset of element pairs to operate on in this step
-- getNextStep returns the multiset of element pairs that need to be operated on in the next step
-- Data.MultiSet.concatMap is a really nifty function. You can think of it as applying some fn to the
-- unique elements of the multiset, each returning their own mini-bag, _and then_ taking the union of all
-- of those for the result
-- So here, applyInsertionRule returns a list of either 1 or 2 element pairs for the next step, but the
-- union means the occurrence count in the resulting bag lines up with what we expect.
getNextStep insertionRules = Data.MultiSet.concatMap (applyInsertionRule insertionRules)

getOccurrences :: MultiSet (Char, Char) -> [(Char, Int)]
-- Taking advantage of concatMap to flatten our bag of element pairs to just all the elements we have total
-- Data.MultiSet.toOccurList takes that element bag, 
-- and returns a list of the element + and its occurrence in the element bag
getOccurrences = Data.MultiSet.toOccurList . Data.MultiSet.concatMap (\(c1, c2) -> [c1, c2])

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let ([template], "" : insertionRules) = splitAt 1 (lines contents)
    -- The initial step of the polymerization process will occur on the pairs of elements derived
    -- from the sliding window of length 2 on the first line of the input file.
    -- From those pairs, new pairs are generated using the insertion rules from the input
    -- That process repeats for the desired number of iterations.
    -- 
    -- We use a multiset (bag) instead of a character list, because the string length blows up
    -- after a few iterations, whereas the bag means we only need to operate on the unique element
    -- pairs per iteration.
    let initialStep = Data.MultiSet.fromList $ window2 template
    let parsedInsertionRules = Data.Map.fromList $ map parseInsertionRule insertionRules

    let iterations = iterate (getNextStep parsedInsertionRules) initialStep
    let result = sort . map snd $ getOccurrences (iterations !! 40)
    -- Math thats probably wrong, but somehow works for the input given to me
    -- If we insert (A1, B), (B, A2) on each part of a single iteration, we'll have double counted B and A2
    -- So, everything except the first character has been double counted
    print $ (last result - head result + 1) `div` 2
