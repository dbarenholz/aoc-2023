import Data.List (intercalate, findIndices, isPrefixOf, tails)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE

main :: IO ()
main = interact (formatOutput . solve . parseInput)

-- | Returns list of strings that are known to be non empty, obtained by splitting `s` on `sep`.
splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of [] -> "" :| []; (part : remainder) -> if part == sep then "" <| splitOn' sep remainder else let head :| tail = splitOn' sep remainder in (part : head) :| tail

-- | Returns list of strings obtained by splitting `s` on `sep`.
splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)

-- | Infix operation for splitting strings
(-|-) :: String -> Char -> [String]
s -|- sep = splitOn sep s

-- | Returns starting indices of `needle` in `s`. Returns the empty list if not found.
findString :: String -> String -> [Int]
findString needle s = findIndices (isPrefixOf needle) (tails s)

-- | List indexing that returns Nothing if index is out of bounds.
--  Otherwise, return `Just (s !! i)`
atIndex :: [a] -> Int -> Maybe a
atIndex s i = if 0 <= i && i < length s then Just (s !! i) else Nothing

-- | Infix operator for indexing 1D arrays to `Maybe`s.
(!!?) :: [a] -> Int -> Maybe a
s !!? i = atIndex s i

-- | List indexing on 2D lists. 
-- Returns Nothing if (either) index is out of bounds.
-- Otherwise, return `Just (mat !! j) !! i`
atIndex2D :: [[a]] -> Int -> Int -> Maybe a
atIndex2D mat i j =
  case atIndex mat j of
    Nothing -> Nothing
    Just x -> x !!? i


type Input = String
type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> Input
parseInput rawInput = map read (rawInput -|- ',')

-- parseTestcases :: String -> [Input]
-- parseTestcases rawInput = map parseInput tail (lines rawInput)

solve :: Input -> Output
solve s = read s
