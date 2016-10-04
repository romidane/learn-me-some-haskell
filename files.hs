-- Just as hGetContents works like getContents but for a specific file,
-- functions like hGetLine, hPutStr, hPutStrLn, hGetChar, and so on work just
-- like their counterparts without the h but take only a handle as a parameter
-- and operate on that specific file instead of on standard input or standard output.
-- For example, putStrLn takes a string and returns an I/O action that will
-- print out that string to the terminal and a newline after it.
-- hPutStrLn takes a handle and a string and returns an I/O action
-- that will write that string to the file associated with the handle and then
-- put a newline after it. In the same vein, hGetLine takes a handle and returns
-- an I/O action that reads a line from its file.

-- Loading files and then treating their contents as strings is so common that
-- we have three nice little functions to make our work even easier:
-- readFile, writeFile, and appendFile.

-- The readFile function has a type signature of
-- readFile :: FilePath -> IO String. 
-- readFile takes a path to a file and returns an I/O action that will read that file (lazily, of course)
-- and bind its contents to something as a string. It’s usually more handy than
-- calling openFile and then calling hGetContents with the resulting handle.

import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
