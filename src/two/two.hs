import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = interact (formatOutput . solve2 . parseInput2)

-- helpers for parsing
splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of [] -> "" :| []; (part : remainder) -> if part == sep then "" <| splitOn' sep remainder else let head :| tail = splitOn' sep remainder in (part : head) :| tail

splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)


-- Part 1: sum all game IDs of valid games
-- (gameID, [(r, g, b), (r, g, b), ...])
type Input1 = [(Int, [(Int, Int, Int)])]
type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

-- | Given input line, return gameID
parseGameID :: String -> Int
parseGameID s = read (head (tail (words (head (splitOn ':' s)))))

-- | Given input line, return ["string describing game"]
gamesOf :: String -> [String]
gamesOf s = splitOn ';' (drop 1 (splitOn ':' s !! 1))

-- | Given "string describing game", return counts (r, g, b) as tuple.
parseGameToTuple :: String -> (Int, Int, Int)
parseGameToTuple game =
  let
    m = Map.fromList (map ((\s -> (head (tail s), read (head s))) . words) (splitOn ',' game))
  in
    (Map.findWithDefault 0 "red" m, Map.findWithDefault 0 "green" m, Map.findWithDefault 0 "blue" m)

-- | Given input line, return [games as tuples (r, g, b)]
parseGamesToTuples :: String -> [(Int, Int, Int)]
parseGamesToTuples s = map parseGameToTuple (gamesOf s)

-- | Given input line, return (gameID, [games as tuples (r, g, b)]).
parseLine1 :: String -> (Int, [(Int, Int, Int)])
parseLine1 s = (parseGameID s, parseGamesToTuples s)

parseInput1 :: String -> Input1
parseInput1 rawInput = map parseLine1 (lines rawInput)

maxR :: Int
maxR = 12

maxG :: Int
maxG = 13

maxB :: Int
maxB = 14

possible :: (Int, Int, Int) -> Bool
possible (r, g, b) = r <= maxR && g <= maxG && b <= maxB

solve1 :: Input1 -> Output
solve1 s = sum (map (\(gameID, games) -> if all possible games then gameID else 0) s)


-- Part 2: sum all "powers" of minimal required items for each game.
--, [ { "red": [1], "green": [2], "blue": [3] }, ... ]
type Input2 = [Map String [Int]]

-- | Given a "game" return a map { "red": [1], "green": [2], "blue": [3] }
parseGameToMap :: String -> Map String [Int]
parseGameToMap game = Map.fromList (map ((\s -> (head (tail s), [read (head s)])) . words) (splitOn ',' game))

-- | Given input line, return { "red": [1...], "green": [2...], "blue": [3...] }
-- the returned map contains all individual values from the input line
parseLineToMap :: String -> Map String [Int]
parseLineToMap line =
  let gamesList = gamesOf line
      maps = map parseGameToMap gamesList
   in Map.unionsWith (++) maps

-- | Given a map of a single game, return "power" of it.
powerOf :: Map String [Int] -> Int
powerOf m =
  let minR = maximum (Map.findWithDefault [1] "red" m)
      minG = maximum (Map.findWithDefault [1] "green" m)
      minB = maximum (Map.findWithDefault [1] "blue" m)
   in minR * minG * minB

-- | Given full input, return list of maps
parseInput2 :: String -> Input2
parseInput2 s = map parseLineToMap (lines s)

solve2 :: Input2 -> Output
solve2 s = sum (map powerOf s)