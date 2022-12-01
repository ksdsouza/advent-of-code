import System.Environment
import Data.List

splitNote :: [String] -> ([String], [String])
splitNote values = (take 10 values, drop 11 values)

data Stencil = Stencil {
    top :: String,
    left :: String,
    right :: String,
    middle :: String,
    bottomLeft :: String,
    bottomRight :: String,
    bottom :: String
} deriving (Show, Eq)

setTop stencil value = stencil {top = [value]}
setBottom stencil value = stencil {bottom = [value]}
setLeft stencil value = stencil {left = [value]}
setRight stencil value = stencil {right = [value]}
setMiddle stencil value = stencil {middle = [value]}
setBottomLeft stencil value = stencil {bottomLeft = [value]}
setBottomRight stencil value = stencil {bottomRight = [value]}

emptyStencil :: Stencil
emptyStencil = Stencil ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g']

is1,is4,is7,is8 :: String -> Bool
is1 = (2 ==) . length
is4 = (4 == ) . length
is7 = (3 == ) . length
is8 = (7 ==) . length

allCharsInString :: String -> String -> Bool
allCharsInString chars value = length value == length chars && all (`elem` value) chars

matches0AgainstStencil :: Stencil -> String -> Bool
matches0AgainstStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) word = allCharsInString [t, l, r, bl, br, b] word
matches0AgainstStencil _ _ = False

matches6AgainstStencil :: Stencil -> String -> Bool
matches6AgainstStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) word = allCharsInString [t, l, m, bl, br, b] word
matches6AgainstStencil _ _ = False

matches9AgainstStencil :: Stencil -> String -> Bool
matches9AgainstStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) word = allCharsInString [t, l, r, m, br, b] word
matches9AgainstStencil _ _ = False

matches2AgainstStencil :: Stencil -> String -> Bool
matches2AgainstStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) word = allCharsInString [t, r, m, bl, b] word
matches2AgainstStencil _ _ = False

matches5AgainstStencil :: Stencil -> String -> Bool
matches5AgainstStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) word = allCharsInString [t, l, m, br, b] word
matches5AgainstStencil _ _ = False

matches3AgainstStencil :: Stencil -> String -> Bool
matches3AgainstStencil (Stencil [t] [l] [r] [m] [bl] [br] [b]) word = length word == 5 && all (`elem` word) [t, r, m, br, b]
matches3AgainstStencil _ _ = False

apply1,apply4,apply7 :: String -> Stencil -> Stencil
apply1 word stencil = stencil {
    top = filter (`notElem` word) (top stencil),
    left = filter (`notElem` word) (left stencil),
    right = filter (`elem` word) (right stencil),
    middle = filter (`notElem` word) (middle stencil),
    bottomLeft = filter (`notElem` word) (bottomLeft stencil),
    bottomRight = filter (`elem` word) (bottomRight stencil),
    bottom = filter (`notElem` word) (bottom stencil)
}

apply4 word stencil = stencil {
    top = filter (`notElem` word) (top stencil),
    left = filter (`elem` word) (left stencil),
    right = filter (`elem` word) (right stencil),
    middle = filter (`elem` word) (middle stencil),
    bottomLeft = filter (`notElem` word) (bottomLeft stencil),
    bottomRight = filter (`elem` word) (bottomRight stencil),
    bottom = filter (`notElem` word) (bottom stencil)
}

apply7 word stencil = stencil {
    top = filter (`elem` word) (top stencil),
    left = filter (`notElem` word) (left stencil),
    right = filter (`elem` word) (right stencil),
    middle = filter (`notElem` word) (middle stencil),
    bottomLeft = filter (`notElem` word) (bottomLeft stencil),
    bottomRight = filter (`elem` word) (bottomRight stencil),
    bottom = filter (`notElem` word) (bottom stencil)
}

filterStencil :: Stencil -> String -> Stencil
filterStencil stencil word
    | is8 word = stencil
    | is1 word = apply1 word stencil
    | is4 word = apply4 word stencil
    | is7 word = apply7 word stencil
    | otherwise = stencil

_applyAllStencils setter accessor stencil = concatMap (allStencils . setter stencil) (accessor stencil)

allStencils :: Stencil -> [Stencil]
allStencils stencil
    | length (top stencil) > 1 = _applyAllStencils setTop top stencil
    | length (left stencil) > 1 = _applyAllStencils setLeft left stencil
    | length (right stencil) > 1 = _applyAllStencils setRight right stencil
    | length (middle stencil) > 1 = _applyAllStencils setMiddle middle stencil
    | length (bottomLeft stencil) > 1 = _applyAllStencils setBottomLeft bottomLeft stencil
    | length (bottomRight stencil) > 1 = _applyAllStencils setBottomRight bottomRight stencil
    | length (bottom stencil) > 1 = _applyAllStencils setBottom bottom stencil
    | otherwise = [stencil | validStencil stencil]

validStencil :: Stencil -> Bool
validStencil stencil@(Stencil [t] [l] [r] [m] [bl] [br] [b]) = all (\grp -> 1 == length grp) ((group . sort) [t,l,r,m,bl,br,b])
validStencil _ = False

stencilWorks :: Stencil -> String -> Bool
stencilWorks stencil@(Stencil [t] [l] [r] [m] [bl] [br] [b]) word
    | is1 word && all (`elem` word) [r, br] = True
    | is4 word && all (`elem` word) [l, r, m, br] = True
    | is7 word && all (`elem` word) [t, r, br] = True
    | is8 word = True
    | matches0AgainstStencil stencil word = True
    | matches6AgainstStencil stencil word = True
    | matches9AgainstStencil stencil word = True
    | matches2AgainstStencil stencil word = True
    | matches3AgainstStencil stencil word = True
    | matches5AgainstStencil stencil word = True
    | otherwise = False
stencilWorks _ _ = False

stencilWorksForLine :: [String] -> Stencil -> Bool
stencilWorksForLine values stencil = all (stencilWorks stencil) values

getValueFromStencil :: Stencil -> String -> Int
getValueFromStencil stencil word
    | is1 word = 1
    | is4 word = 4
    | is7 word = 7
    | is8 word = 8
    | matches0AgainstStencil stencil word = 0
    | matches6AgainstStencil stencil word = 6
    | matches9AgainstStencil stencil word = 9
    | matches2AgainstStencil stencil word = 2
    | matches3AgainstStencil stencil word = 3
    | matches5AgainstStencil stencil word = 5
getValueFromStencil _ _ = error "Invalid"

-- Determines the unique stencil from a single line (pre | character) of the input file
getStencilForLine :: [String] -> Stencil
getStencilForLine decodeLine = head $ filter (stencilWorksForLine decodeLine) allStencilsForLine
    where allStencilsForLine = (allStencils . foldl filterStencil emptyStencil) decodeLine

evaluateLine :: Stencil -> [String] -> Int
evaluateLine stencil = (read . concatMap show) . map (getValueFromStencil stencil)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (map (splitNote . words) . lines) contents

    -- pt1 was just summing these together lol
    -- let num1s = sum $ map (length . filter is1 . snd) parsedContents
    -- let num4s = sum $ map (length . filter is4 . snd) parsedContents
    -- let num7s = sum $ map (length . filter is7 . snd) parsedContents
    -- let num8s = sum $ map (length . filter is8 . snd) parsedContents

    -- Each element of decodeLines contains the 10 strings before the | on a single line of the input file.
    -- Each line can be used to uniquely determine a mapping of characters -> a 7-segment stencil
    let decodeLines = map (sortOn length . fst) parsedContents
    -- Each element of codeLines contains the 4 strings after the | on a single line of the input file
    let codeLines = map snd parsedContents  

    print $ sum $ zipWith evaluateLine (map getStencilForLine decodeLines) codeLines
