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
} deriving (Show, Eq)

emptyStencil :: Stencil
emptyStencil = Stencil ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g'] ['a'..'g']

filterStencil :: Stencil -> String -> Stencil
filterStencil stencil word
    | is8 word = stencil
    | is1 word = apply1 word stencil
    | is4 word = apply4 word stencil
    | is7 word = apply7 word stencil
    | otherwise = stencil

allStencils :: Stencil -> [Stencil]
allStencils stencil@(Stencil [t] [l] [r] [m] [bl] [br] [b]) = if (validStencil stencil) then [stencil] else []
allStencils stencil@(Stencil (t1:_:_) _ _ _ _ _ _) =
    concatMap (\y -> allStencils (stencil {top = [y]})) (top stencil)
allStencils stencil@(Stencil _ (l1:_:_) _ _ _ _ _) =
    concatMap (\y -> allStencils (stencil {left = [y]})) (left stencil)
allStencils stencil@(Stencil _ _ (r1:_:_) _ _ _ _) =
    concatMap (\y -> allStencils (stencil {right = [y]})) (right stencil)
allStencils stencil@(Stencil _ _ _ (m1:_:_) _ _ _) =
    concatMap (\y -> allStencils (stencil {middle = [y]})) (middle stencil)
allStencils stencil@(Stencil _ _ _ _ (bl1:_:_) _ _) =
    concatMap (\y -> allStencils (stencil {bottomLeft = [y]})) (bottomLeft stencil)
allStencils stencil@(Stencil _ _ _ _ _ (br:_:_) _) =
    concatMap (\y -> allStencils (stencil {bottomRight = [y]})) (bottomRight stencil)
allStencils stencil@(Stencil _ _ _ _ _ _ (b:_:_)) =
    concatMap (\y -> allStencils (stencil {bottom = [y]})) (bottom stencil)

validStencil stencil@(Stencil [t] [l] [r] [m] [bl] [br] [b]) = all (\grp -> 1 == (length grp)) ((group . sort) [t,l,r,m,bl,br,b])

stencilWorks (Stencil [t] [l] [r] [m] [bl] [br] [b]) word
    | is1 word && (all (`elem` word) [r, br]) = True
    | is4 word && (all (`elem` word) [l, r, m, br]) = True
    | is7 word && (all (`elem` word) [t, r, br]) = True
    | is8 word = True
    | (length word) == 6 && (all (`elem` word) [t, l, r, bl, br, b]) = True
    | (length word) == 6 && (all (`elem` word) [t, l, m, bl, br, b]) = True
    | (length word) == 6 && (all (`elem` word) [t, l, r, m, br, b]) = True
    | (length word) == 5 && (all (`elem` word) [t, r, m, bl, b]) = True
    | (length word) == 5 && (all (`elem` word) [t, r, m, br, b]) = True
    | (length word) == 5 && (all (`elem` word) [t, l, m, br, b]) = True
    | otherwise = False
stencilWorks stencil word = trace (show stencil) False

stencilWorksForLine values stencil = all (stencilWorks stencil) values

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

checkAgainstStencil :: String -> Stencil -> Int
checkAgainstStencil word (Stencil [t] [l] [r] [m] [bl] [br] [b])
    | is1 word = 1
    | is4 word = 4
    | is7 word = 7
    | is8 word = 8
    | (length word) == 6 && (all (`elem` word) [t, l, r, bl, br, b]) = 0
    | (length word) == 6 && (all (`elem` word) [t, l, m, bl, br, b]) = 6
    | (length word) == 6 && (all (`elem` word) [t, l, r, m, br, b]) = 9
    | (length word) == 5 && (all (`elem` word) [t, r, m, bl, b]) = 2
    | (length word) == 5 && (all (`elem` word) [t, r, m, br, b]) = 3
    | (length word) == 5 && (all (`elem` word) [t, l, m, br, b]) = 5


main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let parsedContents = (map (splitNote . words) . lines) contents

    -- let num1s = sum $ map (length . filter is1 . snd) parsedContents
    -- let num4s = sum $ map (length . filter is4 . snd) parsedContents
    -- let num7s = sum $ map (length . filter is7 . snd) parsedContents
    -- let num8s = sum $ map (length . filter is8 . snd) parsedContents

    let decodeLines = map (sortOn length . fst) parsedContents
    let codeLines = map snd parsedContents

    let stencilForLines = map (allStencils . (foldl filterStencil emptyStencil)) decoders

    let stencils = map (allStencils . (foldl filterStencil emptyStencil)) decoders
    let stencilForLines = map (\decoder -> filter (stencilWorksForLine decoder) stencils) decodeLines
    print $ head stencilForLines
    -- print $ uncurry checkAgainstStencil $ head $ map head $ (map (\(code, stencil) -> map (\c -> (c, stencil)) code) $ zip codes stencils)

