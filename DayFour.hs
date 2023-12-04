import qualified Data.Set as S
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import DayZero

data Card = Card {i :: Integer, cards :: S.Set Integer, winners :: S.Set Integer}

instance Show Card where
    show c = "id: " ++ (show $ i c)

main :: IO ()
main = do
    input <- manuallyParse "d4.txt" "\n" parseCard
    print $ solve1 input
    print $ solve2 (zip input (repeat 1))

solve2 :: [(Card, Int)] -> Int
solve2 ((c, i):xs) =
    let (change, noChange) = splitAt (winningNumbers c) xs
    in i + solve2 (map (\(c', i') -> (c', i' + i)) change ++ noChange)
solve2 _ = 0 

winningNumbers :: Card -> Int
winningNumbers c = S.size $ S.intersection (cards c) (winners c)

solve1 :: [Card] -> Int
solve1 (c:cs) = 
    let rest = solve1 cs
        winners = winningNumbers c
    in  2^winners `div` 2 + rest
solve1 _ = 0

parseCard :: String -> Card
parseCard s = 
    let cardId = read $ takeWhile isDigit (dropWhile (not . isDigit) s)
        (cs:ws:[]) = splitOn " | " (tail $ dropWhile (/= ':') s)
        readC chunk = S.fromList $ map read (filter (/= "") $ splitOn " " chunk)
    in Card cardId (readC cs) (readC ws)
    