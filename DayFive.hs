import DayZero
import Data.List.Split (splitOn)
import Data.Char

type PartMap = Integer -> Maybe Integer
type Map = Integer -> Integer

data Input = Input {seeds :: [Integer], maps :: [Map]}

main :: IO ()
main = do
    inputStr <- splitFile "d5.txt" "\n\n"
    let input = parseInput inputStr
    print $ solve1 input
    print $ length $ getSeedRanges $ seeds input
    print $ solve2 input
    return ()

solve1 :: Input -> Integer
solve1 (Input seeds maps) = minimum $ map (applyMaps maps) seeds

solve2 :: Input -> Integer
solve2 (Input seeds maps) = solve2' 100000000000000 maps (getSeedRanges seeds)
    where solve2' acc maps (s:ss) =
            let acc' = applyMaps maps s
            in if acc < acc' then solve2' acc maps ss else solve2' acc' maps ss
          solve2' acc _ [] = acc

getSeedRanges :: [Integer] -> [Integer]
getSeedRanges (sStart:sLength:seeds) = take (fromIntegral sLength) [sStart..] ++ getSeedRanges seeds
getSeedRanges [] = []

parseInput :: [String] -> Input
parseInput (seedsStr:rest) =
    let seeds = parseSeeds seedsStr
        maps = map (\mapStr -> joinPartMaps (map parseMap (tail mapStr))) (map (splitOn "\n") rest)
    in Input seeds maps

applyMaps :: [Map] -> Integer -> Integer
applyMaps (m:ms) i = applyMaps ms (m i)
applyMaps _ i = i

parseSeeds :: String -> [Integer]
parseSeeds str = 
    let trimmed = dropWhile (not . isDigit) str
    in map read (splitOn " " trimmed)

parseMap :: String -> PartMap
parseMap str = parseMap' $ map read (splitOn " " str)
    where parseMap' (dStart:sStart:range:[]) source
            | source >= sStart && source < sStart + range = Just $ dStart + source - sStart
            | otherwise = Nothing


joinPartMaps :: [PartMap] -> Map
joinPartMaps (f:fs) i = case f i of
    (Just v) -> v
    Nothing -> joinPartMaps fs i
joinPartMaps [] i = i 