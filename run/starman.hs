import           Data.List
import           System.IO
import           System.Random

main :: IO()
main = do
  fileWords <- readFile "swearWords.txt"
  gen <- getStdGen

  let words' = words fileWords
      wordsLen = length words' - 1
      (randomIndex, gen') = randomR (0, wordsLen) gen :: (Int, StdGen)
      randomWord = words' !! randomIndex

  starman randomWord 10

check :: String -> String -> Char -> (Bool, String)
check word display c
  = ( c `elem` word, [if x == c then c else y | (x, y) <- zip word display ])

turn :: String -> String -> Int -> IO()
turn word display n
  | n == 0 = putStrLn $ "You lose the word was (" ++ word ++ ")"
  | word == display = putStrLn $ "(" ++ word ++ ") :: " ++ "You win!"
  | otherwise = mkguess word display n

mkguess :: String -> String -> Int -> IO()
mkguess word display n = do
  putStrLn $ "(" ++ display ++ ") :: You have " ++ show n ++ " guesses left"
  putStr "Enter your guess: "
  q <- getLine
  let (correct, display') = check word display (head q)
  let n' = if correct then n else n-1
  turn word display' n'

starman :: String -> Int -> IO()
starman word = turn word ['-' | x <- word]
