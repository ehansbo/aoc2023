import DayZero
import qualified Data.Map as M
import Data.List.Split

data Input = Input {commands :: [Command], paths :: PathMap}
type PathMap = M.Map Dest (Dest, Dest)
data Command = L | R
    deriving (Read, Show, Eq)
type Dest = String

main :: IO ()
main = do
    (cmdStr:rest:[]) <- splitFile "d8.txt" "\n\n"
    let input = Input (pCmd cmdStr) (pPaths $ splitOn "\n" rest)
    print $ solve1 input
    print $ solve2 input

timeToZ :: [Command] -> PathMap -> Dest -> [Int]
timeToZ allCmds paths start = timeToZ' 0 start allCmds
    where timeToZ' i current (cmd:cmds) =
            let choices = paths M.! current
                next = if cmd == L then fst choices else snd choices
            in if current !! 2 == 'Z' then i:(timeToZ' 1 next cmds) else timeToZ' (i+1) next cmds

solve2 :: Input -> Int
solve2 (Input allCmds paths) =
    let starts = filter (\x -> x !! 2 == 'A') (M.keys paths)
        -- know from tests that loops are as long as the first time from A to Z
        loopLengths = map (head . timeToZ allCmds paths) starts
        primes = concat $ map primeFactors loopLengths
    in product $ uniq $ primes

primeFactors :: Int -> [Int]
primeFactors i =
    let nextFactor = primeFactor 2
    in if nextFactor == i then [i] else nextFactor : (primeFactors (i `div` nextFactor))
        where primeFactor f = if i `mod` f == 0 then f else primeFactor (f+1)

solve1 :: Input -> Int
solve1 (Input allCmds paths) = solve1' 0 "AAA" allCmds
    where solve1' i "ZZZ" _ = i
          solve1' i current (cmd:cmds) = 
            let choices = paths M.! current
                next = if cmd == L then fst choices else snd choices
            in solve1' (i+1) next cmds

pCmd :: String -> [Command]
pCmd = concat . repeat . map (\c -> read [c])

pPaths :: [String] -> PathMap
pPaths = M.fromList . map (\str -> (take 3 str, (take 3 (drop 7 str), take 3 (drop 12 str))))