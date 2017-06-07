-- The monad just like the aplicative functor is an upgrade to the the basic functor.

-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- ghci> fmap (*2) Just 2
-- Just 4
-- ghci> (*) <$> Just 2 <*> Just 8
-- Just 16
-- ghci> (++) <$> Just "klingon" <*> Nothing
-- Nothing
-- ghci> (-) <$> [3,4] <*> [1,2,3]
-- [2,1,0,3,2,1]

-- ghci> Just 4 >>= \a -> return (a * 4) >>= \b -> return (b * 3)
-- Just 48


-------------------------------------------------------
-- The Monad Type Class
-------------------------------------------------------

-- Just like functors have the Functor type class, and applicative functors have the Applicative type class,
-- monads come with their own type class: Monad!

-- class Monad m where
--     return :: a -> m a

--     (>>=) :: m a -> (a -> m b) -> m b

--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y

--     fail :: String -> m a
--     fail msg = error msg


-- The first line says class Monad m where.
-- But wait, didn’t I say that monads are just beefed-up applicative functors?
-- Shouldn’t there be a class constraint in there along the lines of class (Applicative m) = > Monad m where,
-- so that a type must be an applicative functor before it can be made a monad?
-- Well, there should, but when Haskell was made, it hadn’t occurred to people that applicative functors were a good fit for Haskell.
-- But rest assured, every monad is an applicative functor, even if the Monad class declaration doesn’t say so.

-- The first function that the Monad type class defines is return. It’s the same as pure from the Applicative type class.
-- So, even though it has a different name, you’re already acquainted with it. return’s type is (Monad m) => a -> m a.
-- It takes a value and puts it in a minimal default context that still holds that value. In other words,
-- return takes something and wraps it in a monad.

-- The next function is >>=, or bind.
-- It’s like function application, but instead of taking a normal value and feeding it to a normal function,
-- it takes a monadic value (that is, a value with a context) and feeds it to a function that takes
-- a normal value but returns a monadic value.

-- Next up, we have >>. We won’t pay too much attention to it for now because it comes with a default implementation,
-- and it’s rarely implemented when making Monad instances.

-- Normally, passing some value to a function that ignores its parameter and always returns some predetermined value always
-- results in that predetermined value. With monads, however, their context and meaning must be considered as well.
-- Here’s how >> acts with Maybe:

-- ghci> Nothing >> Just 3
-- Nothing
-- ghci> Just 3 >> Just 4
-- Just 4
-- ghci> Just 3 >> Nothing
-- Nothing

-- If we replace >> with >>= \_ ->, it’s easy to see what’s happening.

-- ghci> return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1
-- Nothing


-- The final function of the Monad type class is fail. We never use it explicitly in our code.
-- Instead, it’s used by Haskell to enable failure in a special syntactic construct for monads.



-- Let's examine the instance of the Maybe type class
-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f  = f x
--     fail _ = Nothing

-- Examples of using it moand instace of Maybe
-- ghci> return "WHAT" :: Maybe String
-- Just "WHAT"
-- ghci> Just 9 >>= \x -> return (x*10)
-- Just 90
-- ghci> Nothing >>= \x -> return (x*10)
-- Nothing



-------------------------------------------------------
-- do notation
-------------------------------------------------------

-- Monads in Haskell are so useful that they got their own special syntax, called do notation.

-- Let's examine the same example 3 times

-- foo :: Maybe String
-- foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

-- foo :: Maybe String
-- foo = Just 3   >>= (\x ->
--       Just "!" >>= (\y ->
--       Just (show x ++ y)))

-- To save us from writing all these annoying lambdas, Haskell gives us do notation. It allows us to write the previous piece of code like this:

-- foo :: Maybe String
-- foo = do
--     x <- Just 3
--     y <- Just "!"
--     Just (show x ++ y)

-- do expressions are just different syntax for chaining monadic values.


--------------------------------
-- Pattern Matching and Failure
--------------------------------

-- In do notation, when we bind monadic values to names, we can utilize pattern matching,
-- just as in let expressions and function parameters.
-- Here’s an example of pattern matching in a do expression:

-- justH :: Maybe Char
-- justH = do
--     (x:xs) <- Just "hello"
--     return x

-- We use pattern matching to get the first character of the string "hello", and then
-- we present it as the result. So justH evaluates to Just 'h'.


-- When pattern matching fails in a do expression, the fail function (part of the Monad type class)
-- enables it to result in a failure in the context of the current monad, instead of making the program crash.

-- Here’s its default implementation:

-- fail :: (Monad m) => String -> m a
-- fail msg = error msg

-- So, by default, it does make the program crash.
-- But monads that incorporate a context of possible failure (like Maybe) usually implement it on their own.

-- For Maybe, it’s implemented like so:

-- fail _ = Nothing
-- It ignores the error message and makes a Nothing.
-- So when pattern matching fails in a Maybe value that’s written in do notation,
-- the whole value results in a Nothing. This is preferable to having your program crash.
-- Here’s a do expression with a pattern match that’s bound to fail:

-- wopwop :: Maybe Char
-- wopwop = do
--     (x:xs) <- Just ""
--     return x

-- The pattern matching fails, so the effect is the same as if the whole line with the pattern were replaced with a Nothing.
-- Let’s try this out:

-- ghci> wopwop
-- Nothing

-- The failed pattern matching has caused a failure within the context of our monad instead of causing a program-wide failure,
-- which is pretty neat.


--------------------------------
-- The List Monad
--------------------------------
-- Lists represent nondeterministic values when they’re used as applicatives.
-- A value like 5 is deterministic it has only one result, and we know exactly what it is.
-- On the other hand, a value like [3,8,9] contains several results,
-- so we can view it as one value that is actually many values at the same time.
-- Using lists as applicative functors showcases this nondeterminism nicely.

-- ghci> (*) <$> [1,2,3] <*> [10,100,1000]
-- [10,100,1000,20,200,2000,30,300,3000]

-- All the possible combinations of multiplying elements from the left list with elements from the right list are included in the resulting list.
-- When dealing with nondeterminism, there are many choices that we can make, so we just try all of them.
-- This means the result is a nondeterministic value as well, but it has many more results.


-- This context of nondeterminism translates to monads very nicely.
-- Here’s what the Monad instance for lists looks like:

-- instance Monad [] where
--     return x = [x]
--     xs >>= f = concat (map f xs)
--     fail _ = []


-- Example of feeding a nondeterministic a monadic function
-- ghci> [3,4,5] >>= \x -> [x,-x]
-- [3,-3,4,-4,5,-5]


-- When we used >>= with Maybe, the monadic value was fed into the function while taking care of possible failures.
-- Here, it takes care of non-determinism for us.

-- [3,4,5] is a nondeterministic value, and we feed it into a function that returns a nondeterministic value as well.
-- The result is also nondeterministic, and it features all the possible results of taking elements from the list [3,4,5]
-- and passing them to the function \x -> [x,-x].
-- This function takes a number and produces two results: one negated and one that’s unchanged.
-- So when we use >>= to feed this list to the function, every number is negated and also kept unchanged.
-- The x from the lambda takes on every value from the list that’s fed to it.

-- To see how this is achieved, we can just follow the implementation. First, we start with the list [3,4,5].
-- Then we map the lambda over it and get the following result:

-- [[3,-3],[4,-4],[5,-5]]

-- The lambda is applied to every element, and we get a list of lists.
-- Finally, we just flatten the list, and voilà, we’ve applied a nondeterministic function to a nondeterministic value!


-- Nondeterminism also includes support for failure.
-- The empty list [] is pretty much the equivalent of Nothing, because it signifies the absence of a result.
-- That’s why failing is just defined as the empty list. The error message gets thrown away. Let’s play around with lists that fail:

-- ghci> [] >>= \x -> ["bad","mad","rad"]
-- []
-- ghci> [1,2,3] >>= \x -> []
-- []



-- Just as with Maybe values, we can chain several lists with >>=, propagating the nondeterminism:

-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]


--                     [1, 2]

--         [a, b]                   [a, b]

-- [(1, a)]      [(1,b)]    [(2, a)]      [(2,b)]


-- When you have nondeterministic values interacting, you can view their computation as a tree where every possible
-- result in a list represents a separate branch. Here’s the previous expression rewritten in do notation:

-- listOfTuples :: [(Int, Char)]
-- listOfTuples = do
--     n <- [1,2]
--     ch <- ['a','b']
--     return (n, ch)




---------------------------------------
-- do Notation and List Comprehensions
---------------------------------------

-- Using lists with do notation might remind you of something you’ve seen before.
-- For instance, check out the following piece of code:

-- ghci> [ (n, ch) | n <- [1,2], ch <- ['a','b'] ]

-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- Yes, list comprehensions! In our do notation example, n became every result from [1,2].
-- For every such result, ch was assigned a result from ['a','b'], and then the final line put (n, ch)
-- into a default context (a singleton list) to present it as the result without introducing any additional nondeterminism.
-- In this list comprehension, the same thing happened, but we didn’t need to write return at the end to present (n, ch) as the result,
-- because the output part of a list comprehension did that for us.

-- In fact, list comprehensions are just syntactic sugar for using lists as monads.
-- In the end, list comprehensions and lists in do notation translate to using >>= to do computations that feature nondeterminism.



------------------------------------
-- MonadPlus and the guard Function
------------------------------------

-- The MonadPlus type class is for monads that can also act as monoids. Here is its definition:

-- class Monad m => MonadPlus m where
--     mzero :: m a
--     mplus :: m a -> m a -> m a

-- mzero is synonymous with mempty from the Monoid type class, and mplus corresponds to mappend.
-- Because lists are monoids as well as monads, they can be made an instance of this type class:

-- instance MonadPlus [] where
--     mzero = []
--     mplus = (++)

-- For lists, mzero represents a nondeterministic computation that has no results at all—a failed computation.
-- mplus joins two nondeterministic values into one.

-- The guard function is defined like this:

-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True = return ()
-- guard False = mzero

-- guard takes a Boolean value.
-- If that value is True, guard takes a () and puts it in a minimal default context that still succeeds.
-- If the Boolean value is False, guard makes a failed monadic value. Here it is in action:

-- ghci> guard (5 > 2) :: Maybe ()
-- Just ()
-- ghci> guard (1 > 2) :: Maybe ()
-- Nothing
-- ghci> guard (5 > 2) :: [()]
-- [()]
-- ghci> guard (1 > 2) :: [()]
-- []

-- In the list monad, we use it to filter out nondeterministic computations:

-- ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
sevensOnly :: [Int]
sevensOnly = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]

-- This could also be written as follows without using guard as:

sevensOnly' :: [Int]
sevensOnly' = [1..50] >>= (\x -> (if '7' `elem` show x then return [()] else []) >> return x)


-- Here’s the previous example rewritten in do notation:
sevensOnly'' :: [Int]
sevensOnly'' = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x


-- Had we forgotten to present x as the final result by using return, the resulting list would just be a list of empty tuples.
-- Here’s this again in the form of a list comprehension:

sevensOnly''' :: [Int]
sevensOnly''' = [ x | x <- [1..50], '7' `elem` show x ]

-- So filtering in list comprehensions is the same as using guard.
