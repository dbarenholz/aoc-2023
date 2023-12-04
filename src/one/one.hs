import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)

main :: IO ()
main = interact (formatOutput . solve . parseInput)

type Input = [String]

type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> Input
parseInput = lines

innerProcess :: String -> Char
innerProcess s
  | "one" `isPrefixOf` s = '1'
  | "two" `isPrefixOf` s = '2'
  | "three" `isPrefixOf` s = '3'
  | "four" `isPrefixOf` s = '4'
  | "five" `isPrefixOf` s = '5'
  | "six" `isPrefixOf` s = '6'
  | "seven" `isPrefixOf` s = '7'
  | "eight" `isPrefixOf` s = '8'
  | "nine" `isPrefixOf` s = '9'
  | otherwise = head s

preprocess :: String -> String
preprocess s = map innerProcess (init (tails s))

duplicateSingleCharacter :: String -> String
duplicateSingleCharacter [x] = [x, x]
duplicateSingleCharacter s = s

toFirstAndLastDigit :: String -> String
toFirstAndLastDigit s = [head s, last s]

solve :: Input -> Output
solve s = sum (map (read . toFirstAndLastDigit . duplicateSingleCharacter . filter isDigit . preprocess)  s)
