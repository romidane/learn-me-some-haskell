import Data.Char

-- main = putStrLn "Hello World"

-- the `do` key word allows use to chain multiple
-- main = do
--   putStrLn "Hello, what's your name"
--   name <- getLine
--   putStrLn ("Hey " ++ name ++ ", you rock!")

-- You can think of an I/O action as a box with little feet that will go out into
-- the real world and do something there (like write some graffiti on a wall)
-- and maybe bring back some data. Once it has fetched that data for you, the
-- only way to open the box and get the data inside it is to use the <- construct.

-- main = do
-- no point in binding putStrLn as it return () a unit
    -- foo <- putStrLn "Hello, what's your name?"
    -- name <- getLine
    -- putStrLn ("Hey " ++ name ++ ", you rock!")

-- Notice that we didn’t bind the last putStrLn to anything.
-- That’s because in a do block, the last action cannot be
-- bound to a name as the first two were.
-- do block automatically extracts the value from the last action and yields that as its own result.

----------------------------
-- Using let Inside I/O Actions
----------------------------

-- When using do syntax to glue together I/O actions, we can use let syntax
-- to bind pure values to names. Whereas <- is used to perform I/O actions and
-- bind their results to names, let is used when we just want to give names
-- to normal values inside I/O actions. It’s similar to the let syntax in list comprehensions.

-- take 10 [ (i,j) | i <- [1..],
--                   let k = i*i,
--                   j <- [1..k] ]

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " "
                      ++ bigLastName
                      ++ ", how are you?"

-- You may be wondering when to use <- and when to use let bindings.
-- <- is for performing I/O actions and binding their results to names.
-- map toUpper firstName, however, isn’t an I/O action—it’s a pure expression in Haskell.
-- So you can use <- when you want to bind the results of I/O actions to names,
-- and you can use let bindings to bind pure expressions to names.
