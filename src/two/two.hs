import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import qualified Data.Map as Map


main :: IO ()
main = interact (formatOutput . solve . parseInput)

-- helpers for parsing
splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of [] -> "" :| []; (part : remainder) -> if part == sep then "" <| splitOn' sep remainder else let head :| tail = splitOn' sep remainder in (part : head) :| tail

splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)

-- (gameID, [(r, g, b), (r, g, b), ...])
type Input = [(Int, [(Int, Int, Int)])]

type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

-- | Given input line, return gameID
parseGameID :: String -> Int
parseGameID s = read (head (tail (words (head (splitOn ':' s)))))

-- | Given input line, return ["string describing game"]
gamesOf :: String -> [String]
gamesOf s = splitOn ';' (drop 1 (head (drop 1 (splitOn ':' s))))


-- | Given "string describing game", return counts (r, g, b).
parseGame :: String -> (Int, Int, Int)
parseGame game =
  let
    m = Map.fromList (map (\s -> (head (tail s), read (head s))) (map (\s -> words s) (splitOn ',' game)))
  in
    (Map.findWithDefault 0 "red" m, Map.findWithDefault 0 "green" m, Map.findWithDefault 0 "blue" m)


-- | Given input line, return [games as tuples (r, g, b)]
parseGames :: String -> [(Int, Int, Int)]
parseGames s = map parseGame (gamesOf s)

-- | Given input line, return (gameID, [games as tuples (r, g, b)]).
parseLine :: String -> (Int, [(Int, Int, Int)])
parseLine s = (parseGameID s, parseGames s)

parseInput :: String -> Input
parseInput rawInput = map parseLine (lines rawInput)

-- grabbing r, g, b items is possible if it does not exceed the max set
maxR :: Int
maxR = 12

maxG :: Int
maxG = 13

maxB :: Int
maxB = 14

possible :: (Int, Int, Int) -> Bool
possible (r, g, b) = r <= maxR && g <= maxG && b <= maxB

allPossible :: [(Int, Int, Int)] -> Bool
allPossible games = all (== True) (map possible games)

solve :: Input -> Output
solve s = sum (map (\(gameID, games) -> if allPossible games then gameID else 0) s)
