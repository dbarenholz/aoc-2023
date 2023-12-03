import Data.Char (isDigit)

main :: IO ()
main = interact (formatOutput . solve . parseInput)

type Input = [String]

type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> Input
parseInput = lines

duplicateSingleCharacter :: String -> String
duplicateSingleCharacter [x] = [x, x]
duplicateSingleCharacter s = s

toFirstAndLastDigit :: String -> String
toFirstAndLastDigit s = [head s, last s]

solve :: Input -> Output
solve s = sum (map (read . toFirstAndLastDigit . duplicateSingleCharacter . filter isDigit) s)
