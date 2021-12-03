import System.Environment
import Data.List
import Data.Char ( digitToInt )
import Data.Ord

mostCommonDigit :: [Int] -> Int
-- Given a list of 0s and 1s, we first bucket them into a list of 0s, and a list of 1s.
-- Then, we determine if we have more 0s or 1s, by comparing the size of the two buckets.
-- That returns the larger of the two buckets, so we just grab the first element as the most common digit
-- 
-- Haskell has this neat property where max x y = y, if x <= y; for this problem, that means if the two buckets
-- are the same size, maximumBy will automagically choose the 1s bucket
mostCommonDigit = head . maximumBy (comparing length) . group . sort

leastCommonDigit :: [Int] -> Int
-- Similar to mostCommonDigit, but choosing the smaller of the two lists
leastCommonDigit = head . minimumBy (comparing length) . group . sort

pow2 :: (Integral b, Num a) => b -> a -> a
pow2 exponent base = base * 2 ^ exponent

bin2Dec :: [Int] -> Int
bin2Dec digits = sum . zipWith pow2 [0 ..] $ reverse digits

startsWith :: Eq a => a -> [a] -> Bool
startsWith prefix digits = head digits == prefix

oxygenRating :: [[Int]] -> [Int]
-- Recall from gammaBinary, transposing the numbers will group the ith index of each number together.
-- For each recursive call, we only care about the 1st index of each number.
-- Using the 1st index of each number, we can get the most common value, and we only want to recurse
-- on the numbers which start with that most common value.
-- But we can't recurse on that list directly (infinite recursion!) -- we need to recurse on everything but the first digits
-- for each number.
-- The resulting list is obtained by prefixing the most common digit we obtained from this call onto 
-- the list from the recursive call
oxygenRating [[]] = []
oxygenRating numbers = mostCommon : oxygenRating (map tail numbersStartingWithValue)
    where mostCommon = (mostCommonDigit . head . transpose) numbers
          numbersStartingWithValue = filter (startsWith mostCommon) numbers

co2ScrubberRating :: [[Int]] -> [Int]
co2ScrubberRating [[]] = []
co2ScrubberRating numbers = leastCommon : co2ScrubberRating (map tail numbersStartingWithValue)
    where leastCommon = (leastCommonDigit . head . transpose) numbers
          numbersStartingWithValue = filter (startsWith leastCommon) numbers

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (map (map digitToInt) . words) contents
    -- first transpose the parsed file to group all the 1st indices together, the 2nd indices together, etc
    -- Then, we can then map each index-group down to the most common digit, 
    -- and convert the resulting list of 0s and 1s into a decimal number
    let gammaBinary = (map mostCommonDigit . transpose) parsedContents
    let epsilonBinary = (map leastCommonDigit . transpose) parsedContents
    print $ bin2Dec gammaBinary * bin2Dec epsilonBinary

    let oxygenBinary = oxygenRating parsedContents
    let co2Binary = co2ScrubberRating parsedContents
    print $ bin2Dec oxygenBinary * bin2Dec co2Binary
