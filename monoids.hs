-----------------------------------------------
-- Asscociativity
-----------------------------------------------

-- Consider the following:
-- (*) is a function that takes two numbers and multiplies them.
-- If we multiply some number with a 1, the result is always equal to that number.
-- It doesn’t matter if we do 1 * x or x * 1— the result is always x.
-- Similarly, ++ is a function that takes two things and returns a third.
-- But instead of multiplying numbers, it takes two lists and concatenates them.
-- And much like *, it also has a certain value that doesn’t change the other one when used with ++.
-- That value is the empty list: [].

-- ghci> 4 * 1
-- 4
-- ghci> 1 * 9
-- 9
-- ghci> [1,2,3] ++ []
-- [1,2,3]
-- ghci> [] ++ [0.5, 2.5]
-- [0.5,2.5]

-- It seems that * together with 1 and ++ along with [] share some common properties:

-- The function takes two parameters.

-- The parameters and the returned value have the same type.

-- There exists such a value that doesn’t change other values when used with the binary function.


-- There’s another thing that these two operations have in common that may not be as obvious as our previous observations:
-- When we have three or more values and we want to use the binary function to reduce them to a single result,
-- the order in which we apply the binary function to the values doesn’t matter.
-- For example,
-- whether we use (3 * 4) * 5 or 3 * (4 * 5), the result is 60.
-- The same goes for ++:

-- ghci> (3 * 2) * (8 * 5)
-- 240
-- ghci> 3 * (2 * (8 * 5))
-- 240
-- ghci> "la" ++ ("di" ++ "da")
-- "ladida"
-- ghci> ("la" ++ "di") ++ "da"
-- "ladida"

-- We call this property associativity. * is associative, and so is ++.
-- However, -, for example, is not associative;
-- the expressions (5 - 3) - 4 and 5 - (3 - 4) result in different numbers.

-- By being aware of these properties, we have chanced upon monoids!


-----------------------------------------------
-- The Monoid Type Class
-----------------------------------------------

-- A monoid is made up of an associative binary function
-- and a value that acts as an identity with respect to that function.
-- When something acts as an identity with respect to a function,
-- it means that when called with that function and some other value,
-- the result is always equal to that other value. 1 is the identity with respect to *,
-- and [] is the identity with respect to ++. There are a lot of other monoids to be found in the world of Haskell,
-- which is why the Monoid type class exists. It’s for types that can act like monoids.

-- class Monoid m where
--     mempty :: m
--     mappend :: m -> m -> m
--     mconcat :: [m] -> m
--     mconcat = foldr mappend mempty

-- The Monoid type class is defined in import Data.Monoid.

-- First, we see that only concrete types can be made instances of Monoid,
-- because the m in the type class definition doesn’t take any type parameters.
-- This is different from Functor and Applicative,
-- which require their instances to be type constructors that take one parameter.
-- e.g
--      class (Functor f) => Applicative f where
--          pure :: a -> f a
--          (<*>) :: f (a -> b) -> f a -> f b


-----------------------------------------------
-- mempty
-----------------------------------------------
-- The first function is mempty.
-- It’s not really a function, since it doesn’t take parameters.
-- It’s a polymorphic constant, kind of like minBound from Bounded.
-- mempty represents the identity value for a particular monoid.


-----------------------------------------------
-- mappend
-----------------------------------------------
-- Next up, we have mappend, which, is the binary function.
-- It takes two values of the same type and returns another value of that same type.
-- The decision to call it mappend was kind of unfortunate,
-- because it implies that we’re appending two things in some way.
-- While ++ does take two lists and append one to the other,
-- * doesn’t really do any appending; it just multiplies two numbers together.
-- When you meet other instances of Monoid, you’ll see that most of them don’t append values either.
-- So avoid thinking in terms of appending and just think
-- in terms of mappend being a binary function that takes two monoid values and returns a third.

-----------------------------------------------
-- mconcat
-----------------------------------------------
-- The last function in this type class definition is mconcat.
-- It takes a list of monoid values and reduces them to a single value by using mappend between the list’s elements.
-- It has a default implementation, which just takes mempty as a starting value and folds
-- the list from the right with mappend. Because the default implementation is fine for most instances,
-- we won’t concern ourselves with mconcat too much. When making a type an instance of Monoid,
-- it suffices to just implement mempty and mappend. Although for some instances,
-- there might be a more efficient way to implement mconcat,
-- the default implementation is just fine for most cases.



-----------------------------------------------
-- The Monoid Laws
-----------------------------------------------

-- A value that acts as the identity with respect to the binary function
-- and that the binary function must be associative.
-- It’s possible to make instances of Monoid that don’t follow these rules,
-- but such instances are of no use to anyone because when using the Monoid type class,
-- we rely on its instances acting like monoids. Otherwise, what’s the point?
-- That’s why when making monoid instances, we need to make sure they follow these laws:

-- mempty `mappend` x = x

-- x `mappend` mempty = x

-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
