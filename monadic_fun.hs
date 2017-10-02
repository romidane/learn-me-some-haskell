import           Control.Monad.Writer
import           Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

-- Now our applyLog can work for any monoid.
-- Because the accompanying value can now be any monoid value,
-- we no longer need to think of the tuple as a value and a log;
-- now we can think of it as a value with an accompanying monoid value.
-- For instance, we can have a tuple that has an item name and an item price as the monoid value.

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

-- ghci> ("beans", Sum 10) `applyLog` addDrink
-- ("milk",Sum {getSum = 35})

-- Because the value that addDrink returns is a tuple of type (Food, Price),
-- we can feed that result to addDrink again, so that it tells us what we should
-- drink along with our meal and how much that will cost us. Let’s give it a shot:

-- ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
-- ("beer",Sum {getSum = 65})





---------------------------------------------------------------
-- The Writer Type
---------------------------------------------------------------
-- The Control.Monad.Writer module exports the Writer w a type along with its Monad instance and some useful functions
-- for dealing with values of this type.

-- To attach a monoid to a value, we just need to put them together in a tuple.
-- The Writer w a type is just a newtype wrapper for this. Its definition is very simple:

-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- It’s wrapped in a newtype so that it can be made an instance of Monad
-- and so that its type is separate from a normal tuple.
-- The a type parameter represents the type of the value,
-- and the w type parameter represents the type of the attached monoid value.

-- The Control.Monad.Writer module reserves the right to change the way it internally implements the Writer w a type,
-- so it doesn’t export the Writer value constructor.
-- However, it does export the writer function, which does the same thing that the Writer constructor would do.
-- Use it when you want to take a tuple and make a Writer value from it.

-- Because the Writer value constructor is not exported, you also can’t pattern match against it.
-- Instead, you need to use the runWriter function, which takes a tuple that’s wrapped in a Writer newtype
-- and unwraps it, returning a simple tuple.

-- Its Monad instance is defined like so:

-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)
--     (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- First, let’s examine >>=. Its implementation is essentially the same as applyLog,
-- only now that our tuple is wrapped in the Writer newtype, we need to unwrap it when pattern matching.
-- We take the value x and apply the function f to it. This gives us gives us a Writer w a value,
-- and we use a let expression to pattern match on it.
-- We present y as the new result and use mappend to combine the old monoid value with the new one.
-- We pack that up with the result value in a tuple and then wrap that with the Writer constructor so that
-- our result is a Writer value, instead of just an unwrapped tuple.

-- So, what about return?
-- It must take a value and put it in a default minimal context that still presents that value as the result.
-- What would such a context be for Writer values?
-- If we want the accompanying monoid value to affect other monoid values as little as possible,
-- it makes sense to use mempty.

-- mempty is used to present identity monoid values, such as "" and Sum 0 and empty bytestrings.
-- Whenever we use mappend between mempty and some other monoid value, the result is that other monoid value.
-- So, if we use return to make a Writer value and then use >>= to feed that value to a function,
-- the resulting monoid value will be only what the function returns.

-- ghci> runWriter (return 3 :: Writer String Int)
-- (3,"")
-- ghci> runWriter (return 3 :: Writer (Sum Int) Int)
-- (3,Sum {getSum = 0})
-- ghci> runWriter (return 3 :: Writer (Product Int) Int)
-- (3,Product {getProduct = 1})

-- Because Writer doesn’t have a Show instance,
-- we used runWriter to convert our Writer values to normal tuples that can be shown.
-- For String, the monoid value is the empty string. With Sum, it’s 0,
-- because if we add 0 to something, that something stays the same. For Product, the identity is 1.

-- The Writer instance doesn’t feature an implementation for fail,
-- so if a pattern match fails in do notation, error is called.


---------------------------------------------------------------
-- Using do Notation with Writer
---------------------------------------------------------------

-- Now that we have a Monad instance, we’re free to use do notation for Writer values.
-- It’s handy when we have several Writer values and want to do stuff with them.
-- As with other monads, we can treat them as normal values, and the context gets taken care of for us.
-- In this case, all the monoid values that come attached are mappended, and so are reflected in the final result.

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)


-- logNumber takes a number and makes a Writer value out of it.
-- Notice how we used the writer function to construct a Writer value,
-- instead of directly using the Writer value constructor. For the monoid,
-- we use a list of strings, and we equip the number with a singleton list that just says that we have that number.
-- multWithLog is a Writer value that multiplies 3 and 5 and makes sure that their attached logs are included
-- in the final log. We use return to present a*b as the result.
-- Because return just takes something and puts it in a minimal context,
-- we can be sure that it won’t add anything to the log.

-- ghci> runWriter multWithLog
-- (15,["Got number: 3","Got number: 5"])

-- Sometimes, we just want some monoid value to be included at some particular point.
-- For this, the tell function is useful. It’s part of the MonadWriter type class.
-- In the case of Writer, it takes a monoid value, like ["This is going on"],
-- and creates a Writer value that presents the dummy value () as its result,
-- but has the desired monoid value attached. When we have a monadic value that has () as its result,
-- we don’t bind it to a variable.

-- Here’s multWithLog with some extra reporting included:

-- multWithLog :: Writer [String] Int
-- multWithLog = do
--     a <- logNumber 3
--     b <- logNumber 5
--     tell ["Gonna multiply these two"]
--     return (a*b)

-- It’s important that return (a*b) is the last line,
-- because the result of the last line in a do expression is the result of the whole do expression.
-- Had we put tell as the last line, the result of this do expression would be ().
-- We would lose the result of the multiplication. However, the log would be the same.

-- Here’s this in action:

-- ghci> runWriter multWithLog
-- (15,["Got number: 3","Got number: 5","Gonna multiply these two"])



-------------------------------------------------
-- Adding Logging to Programs
-- -------------------------------------------------
-- Euclid’s algorithm takes two numbers and computes their greatest common divisor—that is,
-- the biggest number that still divides both of them.
-- (Note) Haskell already features the gcd function, which does exactly this

gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

-- The algorithm is very simple.
-- First, it checks if the second number is 0.
-- If it is, then the result is the first number.
-- If it isn’t, then the result is the greatest common divisor of the second number
-- and the remainder of dividing the first number with the second one.
