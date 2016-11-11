import           Control.Exception
import           Data.List
import           System.Directory
import           System.Environment
import           System.IO


-- We’ll start by making a function that takes a command in the form of a string, like "add" or "view",
-- and returns a function that takes a list of arguments and returns an I/O action that does what we want:

dispath :: String -> [String] -> IO ()
dispath "add" = add
dispath "view" = view
dispath "remove" = remove
dispath "bump" = bump
dispatch command = doesntExist command



add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments"


-- Suppose we call our program like this:
-- $ ./todo add todo.txt "Find the magic sword of power"
-- command is "add", and argList is ["todo.txt", "Find the magic sword of power"].
-- That way, the second pattern match of the dispatch function will succeed, and it will return the add function.
-- Finally, we apply that to argList, which results in an I/O action that adds the item to our to-do list.

--  ./todo view todo.txt
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

    putStr $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly two arguments"


--  ./todo remove todo.txt 2
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks

    let number = read numberString
        todoItem = todoTasks !! number
        newTodoItems = unlines $ delete todoItem todoTasks

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName
        )
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName

            putStrLn "\n These are you're TO-DO items:"
            putStr newTodoItems
        )


bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let number = read numberString
        todoTasks = lines contents
        todoItem = todoTasks !! number
        newTodoItems = unlines (todoItem : delete todoItem todoTasks)

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName
        )
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName

            putStrLn "\n These are you're TO-DO items:"
            putStr newTodoItems
        )


doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
    putStrLn $ "The " ++ command ++ " command doesn't exist"

main :: IO ()
main =  do
    (command:argsList) <- getArgs
    dispath command argsList


-- First, we get the arguments and bind them to (command:argList).
-- This means that the first argument will be bound to command,
-- and the rest of the arguments will be bound to argList.
-- In the next line of our main block, we apply the dispatch function to the command,
-- which results in the add, view, or remove function. We then apply that function to argList.


