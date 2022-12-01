import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.Map (Map, (!), member)
import Data.List (sort)
import Data.MultiSet (MultiSet, concatMap, fromList, toOccurList, map, union)
import qualified Data.MultiSet as MultiSet
import qualified Data.List as List
import qualified Data.Map as Map

type InsertionRuleSet = Map (Char, Char) Char

-- Groups adjacent elements of a list together
window2 :: Ord a => [a] -> [(a, a)]
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

getNewItemFromPair :: [(a1, a2)] -> [a2]
getNewItemFromPair [(_, newItem), _] = [newItem]
getNewItemFromPair _ = []

-- Applies one step of the polymerization process.
-- getNextStep takes the insertion rules, and the multiset of element pairs to operate on in this step
-- getNextStep returns the multiset of element pairs that need to be operated on in the next step
-- Data.MultiSet.concatMap is a really nifty function. You can think of it as applying some fn to the
-- unique elements of the multiset, each returning their own mini-bag, _and then_ taking the union of all
-- of those for the result
-- So here, applyInsertionRule returns a list of either 1 or 2 element pairs for the next step, but the
-- union means the occurrence count in the resulting bag lines up with what we expect.
getNextStep :: InsertionRuleSet -> (MultiSet (Char, Char), MultiSet Char) -> (MultiSet (Char, Char), MultiSet Char)
getNextStep insertionRules (pairsToProcess, charCounts) = (pairsToProcessNext, charCounts `union` newPairCounts)
    where pairsAfterInsertion = MultiSet.map (applyInsertionRule insertionRules) pairsToProcess
          pairsToProcessNext = MultiSet.concatMap id pairsAfterInsertion
          newPairCounts = MultiSet.concatMap getNewItemFromPair pairsAfterInsertion

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let ([template], "" : insertionRules) = splitAt 1 (lines contents)
    let initialCounts = MultiSet.fromList template

    -- The initial step of the polymerization process will occur on the pairs of elements derived
    -- from the sliding window of length 2 on the first line of the input file.
    -- From those pairs, new pairs are generated using the insertion rules from the input
    -- That process repeats for the desired number of iterations.
    -- 
    -- We use a multiset (bag) instead of a character list, because the string length blows up
    -- after a few iterations, whereas the bag means we only need to operate on the unique element
    -- pairs per iteration.
    let initialStep = MultiSet.fromList $ window2 template
    let parsedInsertionRules = Map.fromList $ List.map parseInsertionRule insertionRules

    let iterations = List.map snd $ iterate (getNextStep parsedInsertionRules) (initialStep, initialCounts)
    let charOccurrences = sort . List.map snd . MultiSet.toOccurList $ iterations !! 40

    print $ last charOccurrences - head charOccurrences
