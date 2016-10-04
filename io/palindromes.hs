main :: IO ()
main = interact respondPalindromes


respondPalindromes :: String -> String
respondPalindromes =
  unlines . map (\line -> if isPal line then line ++ " is a palindrome" else line ++ " isn't a palindrome" ) . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs
