import           Control.Exception
import           Data.List
import           System.Directory
import           System.IO

main :: IO()
main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        len = length todoTasks
        numberedTasks = zipWith (\num line -> show num ++ " - " ++ line) [0..] todoTasks

    putStrLn "These are you're TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"

    numberedString <- getLine

    let number = read numberedString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks


    -- With error handling

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName
        )
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt"
        )

    -- Without error handling
    -- (tempName, tempHandle) <- openTempFile "." "temp"

    -- hPutStr tempHandle newTodoItems

    -- hClose tempHandle

    -- removeFile "todo.txt"

    -- renameFile tempName "todo.txt"


-- Ok so what's going on here

-- First, we read todo.txt and bind its contents to contents.
-- Then we split the contents into a list of strings,
-- with one line for each string. So todoTasks is now something like this:
-- ["Iron the dishes", "Dust the dog", "Take salad out of the oven"]

-- We zip the numbers from 0 onward and that list with a function that takes
-- a number (like 3) and a string (like "hey") and returns a new string (like "3 - hey").
-- Now numberedTasks looks like this:
-- ["0 - Iron the dishes"
-- ,"1 - Dust the dog"
-- ,"2 - Take salad out of the oven"
-- ]

-- We then use mapM_ putStrLn numberedTasks to print each task on a separate line,
-- ask the user which one to delete, and wait for the user to enter a number.

-- Remember the delete and !! functions from Data.List?
-- !! returns an element from a list with some index.
-- delete deletes the first occurrence of an element in a list and returns
-- a new list without that occurrence.

-- So say we inputed 1 into the console
-- We delete the the first occurrence of "Dust the dog" from todoTasks
-- and then join that into a single line with unlines and name that newTodoItems.

-- Then we use a function from System.IO: openTempFile.
-- Its name is pretty self-explanatory.
-- It takes a path to a temporary directory and a template name for a file and opens a temporary file.
-- We used "." for the temporary directory, because . denotes the current directory
-- on just about any operating system.

-- We used "temp" as the template name for the temporary file,
-- which means that the temporary file will be named temp plus some random characters.
-- It returns an I/O action that makes the temporary file,
-- and the result in that I/O action is a pair of values: the name of the temporary file and a handle.
-- We could just open a normal file called todo2.txt or something like that,
-- but it’s better practice to use openTempFile so you know you’re probably not overwriting anything.

-- Now that we have a temporary file opened, we write newTodoItems to it. The old file is unchanged,
-- and the temporary file contains all the lines that the old one does, except the one we deleted.

-- After that, we close both the original and the temporary files, and remove the original one with removeFile,
-- which takes a path to a file and deletes it.

-- After deleting the old todo.txt, we use renameFile to rename the temporary file to todo.txt.
-- removeFile and renameFile (which are both in System.Directory) take file paths, not handles, as their parameters.


-- Error handling
-- To make sure our temporary file is cleaned up in case of a problem,
-- we’re going to use the bracketOnError function from Control.Exception.
-- It’s very similar to bracket, but whereas the bracket will acquire a resource and then make sure that some
-- cleanup always gets done after we’ve used it,
-- bracketOnError performs the cleanup only if an exception has been raised.

-- We write what we want to happen if an error occurs;
    -- that is, we want to close the temporary handle and remove the temporary file.

-- Finally, we write what we want to do with the temporary file while things are going well,
    -- that is we write the new items, close the temporary handle, remove our current file,
    -- and rename the temporary file.
