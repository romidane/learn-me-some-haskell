

import Data.List
import Data.Char


main :: IO ()

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

-- lowestBound :: Int -> Int
-- lowestBound n = read $ replicate (floor . fromIntegral $ (n `div` 9)) '9' :: Int

lowestBound :: Int -> String
lowestBound n = replicate (floor . fromIntegral $ (n `div` 9)) '9'

firstToN :: Int -> Integer
firstToN n = read $ show (n `mod` 9) ++ lowestBound n :: Integer


-- firstTo :: Int -> Maybe Int
-- firstTo n = find (\x -> digitSum x == n) [1..]
--
-- firstTo2 :: Int -> Maybe Int
-- firstTo2 n = find (\x -> digitSum x == n) [(lowestBound n)..]

main = do
  putStrLn "What number are you thinking of? "
  val <- getLine
  putStrLn ("The answer is " ++ (show $ firstToN (read val :: Int)))
