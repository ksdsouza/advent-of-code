import System.Environment
import Data.List.Split
import Data.List (partition, nub)
import Data.Set (fromList, member)

type Point = (Int, Int)
type Instr = (String, Int)

parsePoint :: String -> Point
parsePoint str = (read x, read y)
    where [x, y] = splitOn "," str

parseInstr :: String -> Instr
parseInstr str = (axis, read value)
    where [axis, value] = splitOn "=" str

takeEvery :: Int -> [a] -> [a]
takeEvery n lst = case drop (n-1) lst of
    y:ys -> y : takeEvery n ys
    [] -> []

window :: Int -> [a] -> [[a]]
window _ [] = []
window n lst = take (n+1) lst : window n (drop (n+1) lst)

dimensions :: (Ord b, Ord a) => [(a, b)] -> (a, b)
dimensions dots = (maximum xs, maximum ys)
    where (xs, ys) = (map fst dots, map snd dots)

reflectOnX :: Num a => a -> (a, b) -> (a, b)
reflectOnX b (x, y) = (2*b - x, y)

reflectOnY :: Num b => b -> (a, b) -> (a, b)
reflectOnY b (x, y) = (x, 2*b - y)

foldPaper :: [Point] -> Instr -> [Point]
foldPaper dots ("x", value) = nub $ left ++ map (reflectOnX value) right
    where (left, right) = partition ((< value) . fst) dots
foldPaper dots ("y", value) = nub $ top ++ map (reflectOnY value) bottom
    where (top, bottom) = partition ((< value) . snd) dots
foldPaper dots _ = error "Invalid instruction"

render :: [Point] -> [String]
render dots = window maxX renderedPoints
    where (maxX, maxY) = dimensions dots
          allPoints = [(x, y) | y <- [0..maxY], x <- [0..maxX]]
          filledPointsSet = fromList dots
          renderedPoints = [ if pt `member` filledPointsSet then '#' else ' ' | pt <- allPoints]

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let [dotsInput, foldInput] = splitOn "\n\n" contents
    let parsedDots = map parsePoint (words dotsInput)
    let parsedInstrs = map parseInstr (takeEvery 3 (words foldInput))
    mapM_ putStrLn $ render $ foldl foldPaper parsedDots parsedInstrs
