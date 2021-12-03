import System.Environment
import Data.List
import Data.Char ( digitToInt )
import Data.Ord

mostSignificantDigit :: [Int] -> Int
mostSignificantDigit = head . maximumBy (comparing length) . group . sort

leastSignificantDigit :: [Int] -> Int
leastSignificantDigit = head . minimumBy (comparing length) . group . sort

pow2 :: (Integral b, Num a) => b -> a -> a
pow2 exponent base = base * 2 ^ exponent

bin2Dec :: [Int] -> Int
bin2Dec digits = sum . zipWith pow2 [0 ..] $ reverse digits

startsWith :: Eq a => a -> [a] -> Bool
startsWith prefix digits = head digits == prefix

rating :: [[Int]] -> ([Int] -> Int) -> [Int]
rating [[]] _ = []
rating numbers evalCriteria = value : rating (map tail numbersStartingWithValue) evalCriteria
    where value = (evalCriteria . head . transpose) numbers
          numbersStartingWithValue = filter (startsWith value) numbers

oxygenRating :: [[Int]] -> [Int]
oxygenRating digits = rating digits mostSignificantDigit

co2ScrubberRating :: [[Int]] -> [Int]
co2ScrubberRating digits = rating digits leastSignificantDigit

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (map (map digitToInt) . words) contents

    let gammaBinary = (map mostSignificantDigit . transpose) parsedContents
    let epsilonBinary = (map leastSignificantDigit . transpose) parsedContents
    print $ bin2Dec gammaBinary * bin2Dec epsilonBinary

    let oxygenBinary = oxygenRating parsedContents
    let co2Binary = co2ScrubberRating parsedContents
    print $ bin2Dec oxygenBinary * bin2Dec co2Binary
