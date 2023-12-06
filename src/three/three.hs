import Data.Char (isDigit)
import Data.List (findIndices, groupBy, isPrefixOf, tails)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)

main :: IO ()
main = interact (formatOutput . solve2 . parseInput2)

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

-- Part 1: find all numbers by concatenating digits in a character grid according to some condition, and sum them all

type Input1 = [Int]

type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

indicesMatrix :: [[Char]] -> [[(Int, Int)]]
indicesMatrix s = [[(i, j) | j <- [0 .. length (head s) - 1]] | i <- [0 .. length s - 1]]

sparsify :: [[Char]] -> [(Char, (Int, Int))]
sparsify s =
  let
    mat = indicesMatrix s
    tmp = zip s mat
  in
    concatMap (\(a, b) -> zip a b) tmp

groupDigits :: [(Char, (Int, Int))] -> [[(Char, (Int, Int))]]
groupDigits = groupBy (\(c, (i, _)) (c', (i', _)) -> i == i' && isDigit c && isDigit c')

isRelevant :: [(Char, (Int, Int))] -> Bool
isRelevant ls =
  let (c, _) = head ls
   in isDigit c

keepDigits :: [[(Char, (Int, Int))]] -> [[(Char, (Int, Int))]]
keepDigits = filter isRelevant

combineMe :: [(Char, (Int, Int))] -> (Int, Int, Int, Int)
combineMe ins =
  let number = read (map fst ins)
      leftX = minimum (map (\(_, (_, x)) -> x) ins)
      rightX = maximum (map (\(_, (_, x)) -> x) ins)
      y = head (map (\(_, (y, _)) -> y) ins)
   in (number, leftX, rightX, y)

collectSurroundingChars :: (Int, Int, Int, Int) -> [[Char]] -> (Int, [Char])
collectSurroundingChars (num, leftx, rightx, y) s =
  let charLeft = [atIndex2D s (leftx - 1) y]
      charRight = [atIndex2D s (rightx + 1) y]
      chars = [atIndex2D s x y' | x <- [leftx - 1 .. rightx + 1], y' <- [y - 1, y + 1]]
   in (num, catMaybes (chars ++ charLeft ++ charRight))

isPartNumber :: (Int, [Char]) -> Bool
isPartNumber (_, chars) =
  let removedDots = filter (/= '.') chars
      removedDigitsAndDots = filter (not . isDigit) removedDots
   in not (null removedDigitsAndDots)

parseInput1 :: String -> Input1
parseInput1 rawInput =
  let s = lines rawInput
      tuples = map combineMe (keepDigits (groupDigits (sparsify s)))
      surroundingChars = map (`collectSurroundingChars` s) tuples
   in map (\(n, chars) -> if isPartNumber (n, chars) then n else 0) surroundingChars

solve1 :: Input1 -> Output
solve1 = sum


-- Part 2: Find all '*' characters 'adjacent to' exactly two numbers in a character grid, then multiply those numbers, and sum everything.
type Input2 = [Int]


isAdjacentTo :: (Int, Int) -> (Int, Int, Int, Int) -> Bool
isAdjacentTo (stary, starx) (_, leftx, rightx, y) =
  let
    isLeft = stary == y && rightx + 1 == starx
    isRight = stary == y && leftx - 1 == starx
    isAbove = stary == y - 1 && any id [x == starx | x <- [leftx-1..rightx+1]]
    isBelow = stary == y + 1 && any id [x == starx | x <- [leftx-1..rightx+1]]
  in
    isLeft || isRight || isAbove || isBelow

getRatio :: (Int, Int) -> [(Int, Int, Int, Int)] -> Int
getRatio star numbers =
  let
    bools = map (isAdjacentTo star) numbers
    keepMe = zip bools numbers
    adjacentNumbers = map (\(_, (n, _, _, _)) -> n) (filter fst keepMe)
  in
    if length adjacentNumbers  /= 2
      then 0
      else product adjacentNumbers

starLocations :: [(Char, (Int, Int))] -> [(Int, Int)]
starLocations chars = map snd (filter (\(c, _) -> c == '*') chars)


parseInput2 :: String -> Input2
parseInput2 rawInput =
  let
    mat = sparsify (lines rawInput)                       -- sparse matrix of input
    digits = map combineMe (keepDigits (groupDigits mat)) -- [(number, leftx, rightx, y)]
    stars = starLocations mat                             -- [(y, x)] for each '*'
  in
    map (`getRatio` digits) stars

solve2 :: Input2 -> Output
solve2 = sum