import Data.Char
import qualified Data.Set as S

import DayZero


type Coord = (Int, Int)
data Number = Number {number :: Int, neighbors :: S.Set Coord}
    deriving (Show)

symbol :: Char -> Bool
symbol c = not $ isDigit c || c == '.'

main :: IO ()
main = do
    -- part 1
    input <- splitFile "d3.txt" "\n"
    let symbols = getSymbols symbol input
    let numbers = getNumbers input
    print $ solve1 symbols numbers
    -- part 2
    let gears = getSymbols (== '*') input
    print $ solve2 gears numbers

solve1 :: S.Set Coord -> [Number] -> Int
solve1 symbols numbers = sum $ map number $ filter (\(Number _ neighbors) -> 0 < S.size (S.intersection neighbors symbols)) numbers

solve2 :: S.Set Coord -> [Number] -> Int
solve2 gears numbers =
    let gears2Numbers = S.map (\gear -> (gear, map number (filter (\(Number _ neighbors) -> gear `S.member` neighbors) numbers))) gears
        relevantGears = S.filter (\(gear, nums) -> length nums == 2) gears2Numbers
        products = S.map (\(_, (num1:num2:[])) -> num1*num2) relevantGears
    in sum products

getSymbols :: (Char -> Bool) -> [String] -> S.Set Coord
getSymbols f = S.fromList . (getSymbols' 0)
    where getSymbols' y (row:rows) =
            let rest = getSymbols' (y+1) rows
            in map (\x -> (snd x, y)) (filter (f . fst) $ zip row [0..]) ++ rest
          getSymbols' _ _ = []

getNumbers :: [String] -> [Number]
getNumbers = getNumbers' 0
    where getNumbers' y (row:rows) =
            let rest = getNumbers' (y+1) rows
            in getNumbersInRow y (zip row [0..]) ++ rest
          getNumbers' _ _ = []

getNumbersInRow :: Int -> [(Char, Int)] -> [Number]
getNumbersInRow y ((c, x):cs)
    | isDigit c = 
        let numberStr = c : takeWhile isDigit (map fst cs)
            restStr = dropWhile (\(c', x) -> isDigit c') cs
        in Number (read numberStr) (getNeighbors (x, y) (length numberStr)) : getNumbersInRow y restStr
    | otherwise = getNumbersInRow y cs
getNumbersInRow _ _ = []

getNeighbors :: Coord -> Int -> S.Set Coord
getNeighbors (originX, originY) len = S.fromList $ filter (\coord -> not (coord `elem` [(x, originY) | x <- [originX..originX+len-1]])) [(x, y) | x <- [originX-1..originX+len], y <- [originY-1..originY+1]]
