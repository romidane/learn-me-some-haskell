-- A program that takes some input and prints out only those lines that are
-- shorter than 10 characters:
-- main = do
--   contents <- getContents
--   putStr $ shortLinesOnly contents


-- The pattern of getting some string from the input, transforming it with a function,
-- and outputting the result is so common that there is a function that makes that job even easier,
-- called interact. interact takes a function of type String -> String as a parameter
-- and returns an I/O action that will take some input, run that function on it,
-- and then print out the function’s result.


main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines


-- Both part of the prelude module
-- lines :: String -> [String]
-- lines "HEllo \n  mate
-- -> ["HEllo ","  mate"]

-- unlines :: [String] -> String
-- unlines ["HEllo ","  mate"]
-- => "HEllo \n  mate\n"
