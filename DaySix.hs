import DayZero
import Data.Char
import Data.List.Split

type Race = (Time, Distance)
type Time = Int
type Distance = Int

main :: IO ()
main = do
    inputStr <- splitFile "d6.txt" "\n"
    let timeDistance = parse inputStr
    print $ solve1 timeDistance
    let timeDistance2 = parse $ map (filter (/= ' ')) inputStr
    print $ solve1 timeDistance2

solve1 :: [Race] -> Int
solve1 races = product $ map solveRace races

solveRace :: Race -> Int
solveRace (time, dist) = 
    let possible = [1..time-1]
        travel time' = time'*(time-time')
    in length $ filter (> dist) $ map travel possible 

parse :: [String] -> [Race]
parse (timeStr:distanceStr:[]) =
    let fix s = map read $ filter (/= []) $ splitOn " " $ dropWhile (not . isDigit) s
    in zip (fix timeStr) (fix distanceStr)
