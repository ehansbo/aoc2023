import Data.Char
import Data.List
import Data.Maybe
import DayZero

digits = zip (["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ++ map show [0..9]) (map show $ concat $ (repeat [0..9]))
reverseDigits = map (\(f, s) -> (reverse f, s)) digits

main :: IO ()
main = do
    input <- splitFile "d1.txt" "\n"
    print $ part1 input
    print $ part2 input

part1 :: [String] -> Integer
part1 = sum . map part1'
    where part1' input = read $ firstDigit input : [firstDigit $ reverse input] 
          firstDigit = head . dropWhile (not . isDigit) 

part2 :: [String] -> Integer
part2 = 
    let part2' input = read $ firstDigit digits input ++ (firstDigit reverseDigits (reverse input))
        firstDigit digits l@(_:xs) =
            let filtered = filter (\(x, y) -> isJust $ stripPrefix x l) digits
            in if length filtered == 1 then snd $ head filtered else firstDigit digits xs
    in sum . map part2'


