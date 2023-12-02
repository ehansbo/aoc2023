import DayZero
import Data.List.Split
import Data.Char
import qualified Data.Map as M

data Color = Blue | Red | Green
    deriving (Show, Ord, Eq)


data Draw = Draw {red :: Integer, blue :: Integer, green :: Integer}
    deriving (Show)
data Game = Game Integer [Draw]
    deriving (Show)

readColor :: String -> Color
readColor "red" = Red
readColor "blue" = Blue
readColor "green" = Green
readColor s = error $ "topkek" ++s

main :: IO ()
main = do
    games <- manuallyParse "d2.txt" "\n" parseGame
    print $ part1 games
    print $ part2 games
    return ()

part1 :: [Game] -> Integer
part1 gs = part1' 0 gs
    where part1' acc ((Game i draws):gs) = if length (filter (\d -> red d > 12 || blue d > 14 || green d > 13) draws) == 0 then part1' (acc+i) gs else part1' acc gs
          part1' acc _ = acc

part2 :: [Game] -> Integer
part2 gs = sum $ map power gs
 where power (Game _ draws) = 
        let maxC f = maximum (map f draws)
        in product $ [maxC red, maxC blue, maxC green]

parseGame :: String -> Game
parseGame str =
    let (gId:rest:[]) = splitOn ": " str
        drawsStr = splitOn "; " rest
        draws = map parseDraw drawsStr
        id = read $ drop 5 gId
    in Game id draws

parseDraw :: String -> Draw
parseDraw str =
    let colors = M.fromList $ map toColor $ map (splitOn " ") $ splitOn ", " str
        toColor (i:c:_) = (readColor c, read i)
        find c = M.findWithDefault 0 c colors
    in Draw (find Red) (find Blue) (find Green)