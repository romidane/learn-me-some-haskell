import           Control.Aplicative
import           Data.Char
import           Data.List

-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

-- The result of mapping something over an I/O action will be an I/O action,
-- so right off the bat, we use the do syntax to glue two actions and make a new one.
-- In the implementation for fmap, we make a new I/O action that first performs the
-- original I/O action and calls its result result. Then we do return (f result).

---- Example ---

-- main :: IO()
-- main = do
--     line <- fmap reverse getLine
--     putStrLn $ "You said " ++ line ++ " backwards!"
--     putStrLn $ "Yes, you really said " ++ line ++ " backwards!"


-- If you ever find yourself binding the result of an I/O action to a name,
-- only to apply a function to that and call that something else, consider using fmap.
-- If you want to apply multiple functions to some data inside a functor,
-- you can declare your own function at the top level, make a lambda function, or,
-- ideally, use function composition:



-- main = do
    -- line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    -- putStrLn line

-- The intersperse '-' . reverse . map toUpper function takes a string,
-- maps toUpper over it, applies reverse to that result,
-- and then applies intersperse '-' to that result.
-- It’s a prettier way of writing the following:

-- (\xs -> intersperse '-' (reverse (map toUpper xs)))


-- Another instance of Functor that we’ve been dealing with all along is (->) r.
-- The function type r -> a can be rewritten as (->) r a,
-- much like we can write 2 + 3 as (+) 2 3. When we look at it as (->) r a,
-- we can see (->) in a slightly different light.
-- It’s just a type constructor that takes two type parameters, like Either.

-- But remember that a type constructor must take exactly one type parameter
-- so it can be made an instance of Functor.
-- That’s why we can’t make (->) an instance of Functor; however,
-- if we partially apply it to (->) r, it doesn’t pose any problems.
-- If the syntax allowed for type constructors to be partially
-- applied with sections (like we can partially apply + by doing (2+),
-- which is the same as (+) 2), we could write (->) r as (r ->).

-- How are functions functors? Let’s take a look at the implementation,
-- which lies in Control.Monad.Instances:

-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))

-- First, let’s think about fmap’s type:
--     fmap :: (a -> b) -> f a -> f b

-- Next, let’s mentally replace each f,
-- which is the role that our functor instance plays, with (->) r.
-- This will let us see how fmap should behave for this particular instance.
-- Here’s the result:

--     fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

-- Now we can write the (->) r a and (->) r b types as infix r -> a and r -> b,
-- as we normally do with functions:

--     fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- ghci> let a = fmap (*) [1,2,3,4]
-- ghci> :t a
-- a :: [Integer -> Integer]
-- ghci> fmap (\f -> f 9) a
-- [9,18,27,36]


----------------------------
-- Applicative Type Class
----------------------------


-- It comes from in the Control.Applicative

-- It defines two functions: pure and <*>. It doesn’t provide a default implementation
-- for either of them, so we need to define them both if we want something to be
-- an applicative functor.

--      class (Functor f) => Applicative f where
--          pure :: a -> f a
--          (<*>) :: f (a -> b) -> f a -> f b

-- pure
-- pure should take a value of any type and return an applicative value with
-- that value inside it. “Inside it” refers to our box analogy again,
-- even though we’ve seen that it doesn’t always stand up to scrutiny.
-- But the a -> f a type declaration is still pretty descriptive.
-- We take a value and we wrap it in an applicative value that has that
-- value as the result inside it. A better way of thinking about pure
-- would be to say that it takes a value and puts it in some sort of
-- default (or pure) context—a minimal context that still yields that value.

-- <*>
-- It’s like fmap :: (a -> b) -> f a-> f b.
-- You can think of the <*> function as sort of a beefed-up fmap.
-- Whereas fmap takes a function and a functor value and applies
-- the function inside the functor value, <*> takes a functor value
-- that has a function in it and another functor, and extracts that
-- function from the first functor and then maps it over the second one.



---------------------------------
-- Maybe the Applicative Functor
---------------------------------

    -- instance Applicative Maybe where
    --     pure = Just
    --     Nothing <*> _ = Nothing
    --     (Just f) <*> something = fmap f something

-- Examples

    -- ghci> Just (+3) <*> Just 9
    -- Just 12

    -- ghci> pure (+3) <*> Just 10
    -- Just 13
    -- ghci> pure (+3) <*> Just 9
    -- Just 12
    -- ghci> Just (++"hahah") <*> Nothing
    -- Nothing
    -- ghci> Nothing <*> Just "woot"
    -- Nothing



----------------------------
-- The Applicative Style
----------------------------

-- With the Applicative type class, we can chain the use of the <*> function,
-- thus enabling us to seamlessly operate on several applicative values instead of just one.
-- For instance, check this out:
    -- ghci> pure (+) <*> Just 3 <*> Just 5
    -- Just 8
    -- ghci> pure (+) <*> Just 3 <*> Nothing
    -- Nothing
    -- ghci> pure (+) <*> Nothing <*> Just 5
    -- Nothing

-- We wrapped the + function inside an applicative value
-- and then used <*> to call it with two parameters, both applicative values.

-- <*> is left-associative, which means that this:
-- pure (+) <*> Just 3 <*> Just 5
-- is the same as this:
-- (pure (+) <*> Just 3) <*> Just 5

-- First, the + function is put in an applicative value—in this case,
-- a Maybe value that contains the function. So we have pure (+), which is Just (+).
-- Next, Just (+) <*> Just 3 happens. The result of this is Just (3+).
-- This is because of partial application. Only applying the + function to 3
-- results in a function that takes one parameter and adds 3 to it.
-- Finally, Just (3+) <*> Just 5 is carried out, which results in a Just 8.

-- pure f <*> x equals fmap f x

-- If we just put a function in a default context and then extract and apply it to a value
-- inside another applicative functor, that’s the same as just mapping that function over
-- that applicative functor. Instead of writing pure f <*> x <*> y <*> ...,
-- we can write fmap f x <*> y <*> ....
-- This is why Control.Applicative exports a function called <$>,
-- which is just fmap as an infix operator. Here’s how it’s defined:

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x

-- Note
-- Remember that type variables are independent of parameter names or other value names.
-- The f in the function declaration here is a type variable with a class constraint
-- saying that any type constructor that replaces f should be in the Functor type class.
-- The f in the function body denotes a function that we map over x.
-- The fact that we used f to represent both of those doesn’t
-- mean that they represent the same thing.

-- By using <$>, the applicative style really shines, because now if we want to apply
-- a function f between three applicative values, we can write f <$> x <*> y <*> z.
-- If the parameters were normal values rather than applicative functors,
-- we would write f x y z

-- ghci> (++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"

-- Before we see how this happens, compare the preceding line with this:
-- ghci> (++) "johntra" "volta"
-- "johntravolta"


-- To use a normal function on applicative functors,
-- just sprinkle some <$> and <*> about, and the function will
-- operate on applicatives and return an applicative.



----------------------------
-- Lists
----------------------------

-- Lists (actually the list type constructor, []) are applicative functors.
-- What a surprise! Here’s how [] is an instance of Applicative:

-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]


-- What about <*>? If the <*> function’s type were limited to only lists,
-- we would get (<*>) :: [a -> b] -> [a] -> [b]. It’s implemented with a list comprehension.
-- <*> must somehow extract the function out of its left parameter and then map it over
-- the right parameter. But the left list can have zero functions, one function,
-- or several functions inside it, and the right list can also hold several values.
-- That’s why we use a list comprehension to draw from both lists.
-- We apply every possible function from the left list to every possible value from
-- the right list. The resulting list has every possible combination of applying a
-- function from the left list to a value in the right one.

-- Example usage

-- [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]

-- [(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]

-- <*> is left-associative, so [(+),(*)] <*> [1,2] happens first,
-- resulting in a list that’s the same as [(1+),(2+),(1*),(2*)],
-- because every function on the left gets applied to every value on the right.
-- Then [(1+),(2+),(1*),(2*)] <*> [3,4] happens, which produces the final result.

-- (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]


-- Using the applicative style on lists is often a good replacement
-- for list comprehensions. If we wanted to see all the possible products
-- of [2,5,10] and [8,10,11], so we could do this:

-- ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
-- [16,20,22,40,50,55,80,100,110]

-- We’re just drawing from two lists and applying a function between every combination
-- of elements. This can be done in the applicative style as well:

-- ghci> (*) <$> [2,5,10] <*> [8,10,11]
-- [16,20,22,40,50,55,80,100,110]

-- This seems clearer to me, because it’s easier to see that we’re
-- just calling * between two nondeterministic computations.
-- If we wanted all possible products of those two lists that are more than 50,
--  we would use the following:

-- filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]

-- It’s easy to see how pure f <*> xs equals fmap f xs with lists.
-- pure f is just [f], and [f] <*> xs will apply every function
-- in the left list to every value in the right one, but there’s just
-- one function in the left list, so it’s like mapping



-------------------------------------
-- IO Is An Applicative Functor, Too
-------------------------------------


-- Another instance of Applicative that we’ve already encountered is IO.
-- This is how the instance is implemented:

-- instance Applicative IO where
--     pure = return
--     a <*> b = do
--         f <- a
--         x <- b
--         return (f x)

-- Since pure is all about putting a value in a minimal context that still holds the value
-- as the result, it makes sense that pure is just return.
-- return makes an I/O action that doesn’t do anything.
-- It just yields some value as its result, without performing any I/O operations
-- like printing to the terminal or reading from a file.

-- With Maybe and [], we could think of <*> as simply extracting a function from its
-- left parameter and then applying it over the right one. With IO, extracting is still
-- in the game, but now we also have a notion of sequencing, because we’re taking
-- two I/O actions and gluing them into one. We need to extract the function from
-- the first I/O action, but to extract a result from an I/O action, it must be performed.

-- myAction :: IO String
-- myAction = do
--     a <- getLine
--     b <- getLine
--     return $ a ++ b

-- This is an I/O action that will prompt the user for two lines and yield as
-- its result those two lines concatenated.

-- myAction' :: IO String
-- myAction' = (++) <$> getLine <*> getLine

-- This is the same thing we did earlier when we were making an I/O action that
-- applied a function between the results of two other I/O actions.

-- main = do
--     a <- (++) <$> getLine <*> getLine
--     putStrLn $ "The two lines concatenated turn out to be: " ++ a



----------------------------
-- Functions As Applicatives
----------------------------
-- No really important but you can you can use it as follows.

-- instance Applicative ((->) r) where
--     pure x = (\_ -> x)
--     f <*> g = \x -> f x (g x)

--- usage
-- ghci> :t (+) <$> (+3) <*> (*100)
-- (+) <$> (+3) <*> (*100) :: (Num a) => a -> a
-- ghci> (+) <$> (+3) <*> (*100) $ 5
-- 508



--------------------------------
-- Zip Lists
--------------------------------

-- instance Applicative ZipList where
--     pure x = ZipList (repeat x)
--     ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- <*> applies the first function to the first value, the second function
-- to the second value, and so on. This is done with zipWith (\f x -> f x) fs xs.
-- Because of how zipWith works, the resulting list will be as long as the
-- shorter of the two lists.

-- pure is also interesting here. It takes a value and puts it in a list that just has
-- that value repeating indefinitely. pure "haha" results in
-- ZipList (["haha", "haha","haha".... This might be a bit confusing, since
-- you’ve learned that pure should put a value in a minimal context that
-- still yields that value. And you might be thinking that an infinite list
-- of something is hardly minimal. But it makes sense with zip lists, because
-- it must produce the value on every position.
-- This also satisfies the law that pure f <*> xs should equal fmap f xs.
-- If pure 3 just returned ZipList [3], pure (*2) <*> ZipList [1,5,10]
-- would result in ZipList [2], because the resulting list of two zipped
-- lists has the length of the shorter of the two. If we zip a finite list
-- with an infinite list, the length of the resulting list will always be
-- equal to the length of the finite list.

-- The ZipList a type doesn’t have a Show instance, so we need to use
-- the getZipList function to extract a raw list from a zip list:

-- Without ZipList
-- ghci> [(+1),(*100),(*5)] <*> [1,2,3]
-- [2,3,4,100,200,300,5,10,15]

-- With ZipList
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]



------------------------------------
-- Useful Functions for Applicatives
------------------------------------

-- Control.Applicative defines a function that’s called liftA2,
-- which has the following type and definition:

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f a b = f <$> a <*> b

-- It just applies a function between two applicatives,
-- hiding the applicative style that we’ve discussed.
-- However, it clearly showcases why applicative functors
-- are more powerful than ordinary functors.

-- Example using a regular functior
-- ghci> fmap (\x -> [x]) (Just 4)
-- Just [4]

-- Example using Aplicative style
-- ghci> Just (:[]) <*> Just 4
-- Just [4]

-- Example using liftA2 vs manually typing it
-- ghci> liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]
-- ghci> (:) <$> Just 3 <*> Just [4]
-- Just [3,4]


-- Let’s try implementing a function that takes a list of applicative values
-- and returns an applicative value that has a list as its result value.

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- Examples

-- Suppose we did this
-- ghci> sequenceA [Just 1, Just 2]
-- Just [1,2]

-- By definition, that’s equal to the following:
-- ghci> (:) <$> Just 1 <*> sequenceA [Just 2]
-- Just [1,2]

-- Breaking this down further, we get this:
-- ghci> (:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
-- Just [1,2]

-- We know that sequenceA [] ends up as being Just [], so this expression is now as follows:
-- (:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])

-- which is this:
-- (:) <$> Just 1 <*> Just [2]

-- Another way to implement sequenceA is with a fold.
-- Remember that pretty much any function where we go over a list element
-- by element and accumulate a result along the way can be implemented
-- with a fold:

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])






