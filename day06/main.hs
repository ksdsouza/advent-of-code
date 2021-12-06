import System.Environment
import Data.List
import Data.Ord
import Debug.Trace
import Data.List.Split

decreaseTimer :: Int -> [Int]
decreaseTimer 0 = [6, 8]
decreaseTimer t = [t - 1]

decreaseTimerWeek :: Int -> [Int]
decreaseTimerWeek 0 = [0, 2]
decreaseTimerWeek 1 = [1, 3]
decreaseTimerWeek 2 = [2, 4]
decreaseTimerWeek 3 = [3, 5]
decreaseTimerWeek 4 = [4, 6]
decreaseTimerWeek 5 = [5, 7]
decreaseTimerWeek 6 = [6, 8]
decreaseTimerWeek 7 = [0]
decreaseTimerWeek 8 = [1]

dt remaining value
    | remaining `mod` 7 == 0 = map (f value) [0..] !! remaining
    | otherwise = error "No!"
    where f n 0 = [n]
          f 7 r = dt (r-7) 0
          f 8 r = dt (r-7) 1
          f n r = dt (r-7) n ++ dt (r-7) (n+2)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let timers = (map read . splitOn "," . head . lines) contents :: [Int]
    let timersAligned = foldl (\x i -> concatMap decreaseTimer x) timers [1..4]
    -- print $ timersAligned
    let fn = dt 252
    let result = concatMap fn timersAligned
    print $ length result

    -- print $ decreaseTimerX 14 3

    -- print $ concatMap (decreaseTimerX 7) [3,4,3,1,2]

