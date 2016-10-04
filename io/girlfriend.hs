-- Reading from a file

import System.IO

main :: IO ()
main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- Lets examine this code so we can understand things

-- openFile
-- openFile :: FilePath -> IOMode -> IO Handle

-- openFile takes a file path and an IOMode and returns an I/O action that will
-- open a file and yield the file’s associated handle as its result.
-- FilePath is just a type synonym for String, defined as follows:

-- type FilePath = String

-- IOMode is a type that’s defined like this:

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- Notice that this type is IOMode and not IO Mode. IO Mode would be the type of
-- I/O action that yields a value of some type Mode as its result. IOMode is just a simple enumeration.


-- hGetContents
-- hGetContents :: Handle -> IO String

-- data Handle = GHC.IO.Handle.Types.FileHandle FilePath

-- It takes a Handle, so it knows which file to get the contents from, and
-- returns an IO String—an I/O action that holds contents of the file as its result.
-- This function is pretty much like getContents. The only difference is that getContents
-- will automatically read from the standard input (that is, from the terminal),
-- whereas hGetContents takes a file handle that tells it which file to read from.
-- In all other respects, they work the same.

-- Just like getContents, hGetContents won’t attempt to read all the file at
-- once and store it in memory but will read the content only as needed.
-- This is really cool because we can treat contents as the whole content of the file,
-- but it’s not really loaded in memory. So if this were a really huge file,
-- doing hGetContents wouldn’t choke up our memory.

-- Note the difference between a handle and the actual contents of the file.
-- A handle just points to our current position in the file. The contents are
-- what’s actually in the file. If you imagine your whole filesystem as a really
-- big book, the handle is like a bookmark that shows where you’re currently reading


-- Using the withFile Function

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- main = do
--     withFile "girlfriend.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)

 -- The withFile function makes sure that despite an exception being raised, the file handle is closed.


------ Bracket
-- Control.Exception module offers the bracket function. It has the following type signature:

-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile name mode f = bracket (openFile name mode)
    -- (\handle -> hClose handle)
    -- (\handle -> f handle)

-- The first parameter that we pass to bracket opens the file, and its result
-- is a file handle. The second parameter takes that handle and closes it.
-- bracket makes sure that this happens even if an exception is raised.
-- Finally, the third parameter to bracket takes a handle and applies the
-- function f to it, which takes a file handle and does stuff with that handle,
-- like reading from or writing to the corresponding file.
