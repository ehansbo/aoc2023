import DayZero
import Data.List
import Data.List.Split

type Card = Char
type Bid = Int
type Hand = ([Card], Bid)

cards :: [Card]
cards = "AKQJT98765432"

cards2 :: [Card]
cards2 = "AKQT98765432J"

main :: IO ()
main = do
    input <- manuallyParse "d7.txt" "\n" parse
    print $ solve input score1
    print $ solve input score2


solve :: [Hand] -> ([Card] -> Int) -> Int
solve hands scoreF = sum $ map (\(a, b) -> a*b) $ zip [1..] $ map snd $ sortBy (\h1 h2 -> compare (scoreF $ fst h1) (scoreF $ fst h2)) hands

parse :: String -> Hand
parse str = 
    let (cStr:bStr:[]) = splitOn " " str
    in (cStr, read bStr)

score1 :: [Card] -> Int
score1 hand = 14^6 * (typScore hand) + (sum $ map (\(x, y) -> 14^x * y) $ zip [5,4..] $ map cScore hand)

score2 :: [Card] -> Int
score2 hand = 14^6 * (maximum $ map typScore (getPermutations hand)) +  (sum $ map (\(x, y) -> 14^x * y) $ zip [5,4..] $ map cScore2 hand)

getPermutations :: [Card] -> [[Card]]
getPermutations cs = map getPermutations' (filter (/= 'J') cards)
    where getPermutations' c = replace c cs

replace :: Card -> [Card] -> [Card]
replace c (x:xs)
    | x == 'J' = c:(replace c xs)
    | otherwise = x:(replace c xs)
replace _ [] = []

typScore :: [Card] -> Int
typScore cs =
    let grouped = sortBy (\e1 e2 -> compare (length e2) (length e1)) $ group $ sort cs
    in 
        if length grouped == 1 then 7
        else if length grouped == 2 && (length (grouped !! 0) == 4) then 6
        else if length grouped == 2 then 5
        else if length grouped == 3 && length (grouped !! 0) == 3 then 4
        else if length grouped == 3 then 3
        else if length grouped == 4 then 2
        else 1


cScore :: Card -> Int
cScore card = let (Just score) = elemIndex card (reverse cards) in score

cScore2 :: Card -> Int
cScore2 card = let (Just score) = elemIndex card (reverse cards2) in score