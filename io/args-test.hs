import           Data.List
import           System.Environment


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are: "
    mapM_ putStrLn args
    putStrLn "The program name is: "
    putStrLn progName

-- First, we bind the command-line arguments to args and program name to progName.
-- Next, we use putStrLn to print all the program’s arguments and then the name of the program itself.

-- Usage
-- ./arg-test first second w00t "multi word arg"
-- The arguments are:
-- first
-- second
-- w00t
-- multi word arg
-- The program name is:
-- arg-test

