import System.Environment
import Data.List
import Data.List.Split
import Data.Map

-- FishMap maps the timer value to the number of fishes with that timer value
type FishMap = Map Int Int

getNextTimerValueForKey :: FishMap -> Int -> Int
getNextTimerValueForKey values 8 = Data.Map.findWithDefault 0 0 values
getNextTimerValueForKey values 6 = Data.Map.findWithDefault 0 7 values + Data.Map.findWithDefault 0 0 values
getNextTimerValueForKey values n = Data.Map.findWithDefault 0 (n+1) values

decreaseTimers :: FishMap -> FishMap
decreaseTimers fishMap = Data.Map.mapWithKey (\key count -> getNextTimerValueForKey fishMap key) fishMap

emptyFishMap :: FishMap
emptyFishMap = Data.Map.fromList (zip [0..] (replicate 9 0))

runSimulation :: FishMap -> Int -> FishMap
runSimulation fishMap days = iterate decreaseTimers fishMap !! days

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (Data.List.map read . splitOn "," . head . lines) contents :: [Int]

    let inputFishMap = Data.Map.fromList $ (Data.List.map (\values -> (head values, length values)) . group . sort) parsedContents
    let fishMap = Data.Map.unionWith (+) inputFishMap emptyFishMap

    let resultingFishMap = runSimulation fishMap 256
    let result = Data.Map.foldl (+) 0 resultingFishMap
    print result
