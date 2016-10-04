-- Input Redirection
-- Many interactive programs get the user’s input via the keyboard.
-- However, it’s often more convenient to get the input by feeding the contents
-- of a text file to the program. To achieve this, we use input redirection.

import Control.Monad
import Data.Char

-- main = forever $ do
--   l <- getLine
--   putStrLn $ map toUpper l


-- Runing this example
-- ./capslocker < haiku.txt
-- Instead of inputting lines via the keyboard, we’ll have haiku.txt be the
-- input by redirecting it into our program. To do that, we add a < character
-- after our program name and then specify the file that we want to act as the
-- input.

-------------------------------------
-- Getting Strings from Input Streams
-------------------------------------
-- Let’s take a look at an I/O action that makes processing input streams easier
-- by allowing us to treat them as normal strings: getContents.
-- getContents reads everything from the standard input until it encounters an end-of-file character.
-- Its type is getContents :: IO String.
-- What’s cool about getContents is that it does lazy I/O.
-- This means that when we do foo <- getContents, getContents doesn’t
-- read all of the input at once, store it in memory, and then bind it to foo.
-- No, getContents is lazy! It will say, “Yeah yeah, I’ll read the input from
-- the terminal later as we go along, when you really need it!”

main = do
  contents <- getContents
  putStr $ map toUpper contents

-- When the result of getContents is bound to contents, it’s not represented
-- in memory as a real string, but more like a promise that the string will be
-- produced eventually. When we map toUpper over contents, that’s also a promise
-- to map that function over the eventual contents. Finally, when putStr happens,
-- it says to the previous promise, “Hey, I need a caps-locked line!” It doesn’t have any lines yet, so it says to contents, “How about getting a line from the terminal?” And that’s when getContents actually reads from the terminal and gives a line to the code that asked it to produce something tangible. That code then maps toUpper over that line and gives it to putStr, which prints the line. And then putStr says, “Hey, I need the next line—come on!” This repeats until there’s no more input, which is signified by an end-of-file character.
