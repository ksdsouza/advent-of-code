import System.Environment
import Data.List
import Debug.Trace
-- import Data.List.Split

splitNote :: [String] -> ([String], [String])
splitNote values = (take 10 values, drop 11 values)



is1 :: String -> Bool
is1 = (2 ==) . length

apply1 word stencil =
    stencil {
        top = filter (`notElem` word) (top stencil),
        left = filter (`notElem` word) (left stencil),
        right = filter (`elem` word) (right stencil),
        middle = filter (`notElem` word) (middle stencil),
        bottomLeft = filter (`notElem` word) (bottomLeft stencil),
        bottomRight = filter (`elem` word) (bottomRight stencil),
        bottom = filter (`notElem` word) (bottom stencil)}

apply4 word stencil =
    stencil {
        top = filter (`notElem` word) (top stencil),
        left = filter (`elem` word) (left stencil),
        right = filter (`elem` word) (right stencil),
        middle = filter (`elem` word) (middle stencil),
        bottomLeft = filter (`notElem` word) (bottomLeft stencil),
        bottomRight = filter (`elem` word) (bottomRight stencil),
        bottom = filter (`notElem` word) (bottom stencil)}

apply7 word stencil =
    stencil {
        top = filter (`elem` word) (top stencil),
        left = filter (`notElem` word) (left stencil),
        right = filter (`elem` word) (right stencil),
        middle = filter (`notElem` word) (middle stencil),
        bottomLeft = filter (`notElem` word) (bottomLeft stencil),
        bottomRight = filter (`elem` word) (bottomRight stencil),
        bottom = filter (`notElem` word) (bottom stencil)}

is4 :: String -> Bool
is4 = (4 == ) . length

is7 = (3 == ) . length

is8 = (7 ==) . length

data Stencil = Stencil {
    top :: String,
    left :: String,
    right :: String,
    middle :: String,
    bottomLeft :: String,
    bottomRight :: String,
    bottom :: String
} deriving (Show)

emptyStencil :: Stencil
emptyStencil = Stencil ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g']

filterStencil :: Stencil -> String -> Stencil
filterStencil stencil word
    | is8 word = stencil
    | is1 word = apply1 word stencil
    | is4 word = apply4 word stencil
    | is7 word = apply7 word stencil
    | otherwise = stencil

finalizeStencil :: Stencil -> Stencil
finalizeStencil stencil = stencil {
    left = [head (left stencil)],
    middle = tail (left stencil),
    right = [head (right stencil)],
    bottomRight = tail (right stencil),
    bottomLeft = [head (bottomLeft stencil)],
    bottom = tail (bottomLeft stencil)
}

verifyStencil :: Stencil -> Bool
verifyStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) = False
verifyStencil _ = True

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (map (splitNote . words) . lines) contents

    let num1s = sum $ map (length . filter is1 . snd) parsedContents
    let num4s = sum $ map (length . filter is4 . snd) parsedContents
    let num7s = sum $ map (length . filter is7 . snd) parsedContents
    let num8s = sum $ map (length . filter is8 . snd) parsedContents

    let ws = map (sortOn length . fst) parsedContents


    let stencils = map (finalizeStencil . foldl filterStencil emptyStencil) ws
    print $ stencils

    -- let largestValue = maximum parsedContents

    -- print $ (minimum . map (calculateCost constantRateCost parsedContents)) [0..largestValue]
    -- print $ (minimum . map (calculateCost linearRateCost parsedContents)) [0..largestValue]

    -- Taking the median for pt1/mean for pt2 is the optimized version of the bruteforce approach above
    -- print $ calculateCost constantRateCost parsedContents (median parsedContents)
    -- print $ calculateCost linearRateCost parsedContents (mean parsedContents)
