import Control.Monad

------------------------
-- Putting It in Reverse
------------------------
main = do
  putStrLn "Insert some text"
  line <- getLine

  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main


reverseWords :: String -> String
reverseWords = unwords . map reverse . words


-- Our reverseWords function is just a normal function. It takes a string like
-- "hey there man" and applies words to it to produce a list of words like
-- ["hey","there","man"]. We map reverse over the list, getting
-- ["yeh","ereht","nam"], and then we put that back into one string by using unwords.
-- The final result is "yeh ereht nam".

-- From Data.Char
-- words :: String -> [String]
-- unwords :: [String] -> String



-- main = do
--     a <- return "hell"
--     b <- return "yeah!"
--     putStrLn $ a ++ " " ++ b
-- We can use return in combination with <- to bind stuff to names:
-- So you see, return is sort of the opposite of <-. While return takes a value
-- and wraps it up in a box, <- takes a box (and performs it) and takes the
-- value out of it, binding it to a name. But doing this is kind of redundant,
-- especially since you can use let in do blocks to bind to names, like so:

-- main = do
--     let a = "hell"
--         b = "yeah"
--     putStrLn $ a ++ " " ++ b

-- If null line is True, the code after the then is executed: return ().
-- You might have used a return keyword in other languages to return from
-- a subroutine or function. But return in Haskell is nothing like the return
-- in most other languages.

-- In Haskell (and in I/O actions specifically), return makes an I/O action out of a pure value.
-- Returning to the box analogy for I/O actions, return takes a value and wraps it up in a box.
-- The resulting I/O action doesn’t actually do anything; it just yields that value as its result.
-- So in an I/O context, return "haha" will have a type of IO String.

-- Unlike in other languages, using return doesn’t cause the I/O do block to end
-- in execution. For instance, this program will quite happily continue all the way
-- to the last line:

-- main = do
--     return ()
--     return "HAHAHA"
--     line <- getLine
--     return "BLAH BLAH BLAH"
--     return 4
--     putStrLn line

-- When dealing with I/O do blocks, we mostly use return either because we need to
-- create an I/O action that doesn’t do anything or because we don’t want the I/O
-- action that’s made up from a do block to have the result value of its last action.



----------------------------
-- Some Useful I/O Functions
----------------------------

---------------------
-- putStr
---------------------
-- putStr is much like putStrLn, in that it takes a string as a parameter and
-- returns an I/O action that will print that string to the terminal.
-- However, putStr doesn’t jump into a new line after printing out the string,
-- whereas putStrLn does. For example, look at this code:

-- main = do
--     putStr "Hey, "
--     putStr "I'm "
--     putStrLn "Andy!"

-- => Hey, I'm Andy!

---------------------
-- putChar
---------------------
-- The putChar function takes a character and returns an I/O action that
-- will print it to the terminal:

-- main = do
--     putChar 't'
--     putChar 'e'
--     putChar 'h'

-- putStr can be defined recursively by the help of putChar
putStr' :: String -> IO ()
putStr' [] = return ()
putStr` (x:xs) = do
  putChar x
  putStr xs

---------------------
-- print
---------------------
-- print takes a value of any type that’s an instance of Show
-- (meaning that we know how to represent it as a string),
-- applies show to that value to “stringify” it, and then outputs that string
-- to the terminal. Basically, it’s just putStrLn . show.
-- It first runs show on a value, and then feeds that to putStrLn,
-- which returns an I/O action that will print out our value.

print' :: (Show a) => a -> IO()
print' = putStrLn . show

-- When we want to print out strings, we usually use putStrLn
-- because we don’t want the quotes around them. However,
-- for printing out values of other types to the terminal,
-- print is used the most often.

-- main = do
--     print True
--     print 2
--     print "haha"
--     print 3.2
--     print [3,4,3]


---------------------
-- when
---------------------
-- The when function is found in Control.Monad (to access it, use import Control.Monad).
-- It’s interesting because in a do block, it looks like a flow-control statement,
-- but it’s actually a normal function.

-- when takes a Bool and an I/O action, and if that Bool value is True, it returns the same I/O action that we supplied to it. However, if it’s False, it returns the return () action, which doesn’t do anything.

-- main = do
--     input <- getLine
--     when (input == "SWORDFISH") $ do
--         putStrLn input

-- Without when, we would need to write the program like this:

-- main = do
--     input <- getLine
--     if (input == "SWORDFISH")
--         then putStrLn input
--         else return ()


----------------
-- sequence
----------------
-- The sequence function takes a list of I/O actions and returns an I/O action
-- that will perform those actions one after the other.
-- The result that this I/O action yields will be a list of the results of all
-- the I/O actions that were performed. For instance, we could do this:

-- main = do
--     a <- getLine
--     b <- getLine
--     c <- getLine
--     print [a,b,c]

-- Or we could do this:

-- main = do
--     rs <- sequence [getLine, getLine, getLine]
--     print rs

-- The results of both these versions are exactly the same.
-- sequence [getLine, getLine, getLine] makes an I/O action that will perform getLine three times.
-- If we bind that action to a name, the result is a list of all the results.

-- A common pattern with sequence is when we map functions like print or putStrLn over lists.
-- Executing map print [1,2,3,4] won’t create an I/O action,
-- but instead will create a list of I/O actions.
-- Effectively, this is the same as writing this:
-- [print 1, print 2, print 3, print 4]

-- If we want to transform that list of I/O actions into an I/O action,
-- we must sequence it:
-- sequence $ map print [1,2,3,4,5]
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]


---------------
-- mapM
---------------
-- Because mapping a function that returns an I/O action over a list
-- and then sequencing it is so common, the utility functions mapM and mapM_ were introduced.
-- mapM takes a function and a list, maps the function over the list, and then sequences it.
-- mapM_ does the same thing, but it throws away the result later.
-- We usually use mapM_ when we don’t care what result our sequenced I/O actions have.
-- Here’s an example of mapM:

-- mapM print [1,2,3]
--> 1
--> 2
--> 3
--> [(),(),()]

-- mapM_ print [1,2,3]
--> 1
--> 2
--> 3


----------------
-- forever
----------------
-- The forever function takes an I/O action and returns an I/O action
-- that just repeats the I/O action it got forever.
-- It’s located in Control.Monad. The following little program will indefinitely
-- ask the user for some input and spit it back in all uppercase characters:

-- import Control.Monad
-- import Data.Char
--
-- main = forever $ do
--     putStr "Give me some input: "
--     l <- getLine
--     putStrLn $ map toUpper l

-- forM
-- forM (located in Control.Monad) is like mapM, but its parameters are switched around.
-- The first parameter is the list, and the second is the function to map over that list,
-- which is then sequenced. Why is that useful? Well, with some creative use of lambdas and do notation,
-- we can do stuff like this:
main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number "
                   ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors


-- The (\a -> do ... ) lambda is a function that takes a number and returns an I/O action.
-- Notice that we call return color in the inside do block.
-- We do that so that the I/O action that the do block defines yields the string
-- that represents our color of choice. We actually did not have to do that though,
-- since getLine already yields our chosen color, and it’s the last line in the do block.
-- Doing color <- getLine and then return color is just unpacking the result
-- from getLine and then repacking it—it’s the same as just calling getLine.

-- You can think of forM as saying, “Make an I/O action for every element in this list.
-- What each I/O action will do can depend on the element that was used to make the action.
-- Finally, perform those actions and bind their results to something.”
-- (Although we don’t need to bind it; we could also just throw it away.)

-- We could have actually achieve the same result without forM,
-- but using forM makes the code more readable. Normally, we use forM when we want
-- to map and sequence some actions that we define on the spot using do notation.
