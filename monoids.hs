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



----------------------------------------
-- Some monoids
----------------------------------------

----------------------------
-- Lists Are Monoids
----------------------------

-- instance Monoid [a] where
--     mempty = []
--     mappend = (++)


-- Lists are an instance of the Monoid type class, regardless of the type of the elements they hold.
-- Notice that we wrote instance Monoid [a] and not instance Monoid [],
-- because Monoid requires a concrete type for an instance.

-- ghci> [1,2,3] `mappend` [4,5,6]
-- [1,2,3,4,5,6]
-- ghci> ("one" `mappend` "two") `mappend` "tree"
-- "onetwotree"
-- ghci> "one" `mappend` ("two" `mappend` "tree")
-- "onetwotree"
-- ghci> "one" `mappend` "two" `mappend` "tree"
-- "onetwotree"
-- ghci> "pang" `mappend` mempty
-- "pang"
-- ghci> mconcat [[1,2],[3,6],[9]]
-- [1,2,3,6,9]
-- ghci> mempty :: [a]
-- []


-- The monoid laws do indeed hold for the list instance.
-- When we have several lists and we mappend (or ++) them together,
-- it doesn’t matter which ones we do first, because they’re just joined at the ends anyway.
-- Also, the empty list acts as the identity, so all is well.

-- Notice that monoids don’t require that a `mappend` b be equal to b `mappend` a.
-- In the case of the list, they clearly aren’t:

-- ghci> "one" `mappend` "two"
-- "onetwo"
-- ghci> "two" `mappend` "one"
-- "twoone"

-- And that’s okay. The fact that for multiplication 3 * 5 and 5 * 3 are the same is just a property of multiplication,
-- but it doesn’t hold for all (and indeed, most) monoids.



----------------------------
-- Product and Sum
----------------------------
-- Another way for numbers to be monoids is to have the binary function be + and the identity value be 0:
-- ghci> 0 + 4
-- 4
-- ghci> 5 + 0
-- 5
-- ghci> (1 + 3) + 5
-- 9
-- ghci> 1 + (3 + 5)
-- 9

-- The monoid laws hold, because if you add 0 to any number, the result is that number.
-- And addition is also associative, so we have no problems there.

-- With two equally valid ways for numbers to be monoids, which way do we choose?
-- Well, we don’t have to pick. Remember that when there are several ways
-- for some type to be an instance of the same type class,
-- we can wrap that type in a newtype and then make the new type
-- an instance of the type class in a different way.

-- The Data.Monoid module exports two types for this: Product and Sum. Product is defined like this:

-- newtype Product a =  Product { getProduct :: a }
--     deriving (Eq, Ord, Read, Show, Bounded)

-- It’s simple—just a newtype wrapper with one type parameter along with some derived instances.
-- Its instance for Monoid goes something like this:

-- instance Num a => Monoid (Product a) where
--     mempty = Product 1
--     Product x `mappend` Product y = Product (x * y)

-- mempty is just 1 wrapped in a Product constructor.
-- mappend pattern matches on the Product constructor, multiplies the two numbers, and then wraps the resulting number.
-- As you can see, there’s a Num a class constraint.
-- This means that Product a is an instance of Monoid for all a values that are already an instance of Num.
-- To use Product a as a monoid, we need to do some newtype wrapping and unwrapping:

-- ghci> getProduct $ Product 3 `mappend` Product 9
-- 27
-- ghci> getProduct $ Product 3 `mappend` mempty
-- 3
-- ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
-- 24
-- ghci> getProduct . mconcat . map Product $ [3,4,2]
-- 24


----------------------------
-- Any and All
----------------------------

-- Another type that can act like a monoid in two distinct but equally valid ways is Bool.
-- The first way is to have the function ||, which represents a logical OR,
-- act as the binary function along with False as the identity value. With the logical OR,
-- if any of the two parameters is True, it returns True; otherwise, it returns False.
-- So if we use False as the identity value, OR will return False when used with False and True when used with True.
-- The Any newtype constructor is an instance of Monoid in this fashion.
-- It’s defined like this:

-- newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)

-- Its instance looks like this:
-- instance Monoid Any where
--         mempty = Any False
--         Any x `mappend` Any y = Any (x || y)

-- It’s called Any because x `mappend` y will be True if any one of those two is True.

-- ghci> getAny $ Any True `mappend` Any False
-- True
-- ghci> getAny $ mempty `mappend` Any True
-- True
-- ghci> getAny . mconcat . map Any $ [False, False, False, True]
-- True
-- ghci> getAny $ mempty `mappend` mempty
-- False


----------------------------
-- Any and All
----------------------------
-- Remember the Ordering type?
-- It’s used as the result when comparing things,
-- and it can have three values: LT, EQ, and GT, which stand for less than, equal, and greater than, respectively.

-- ghci> 1 `compare` 2
-- LT
-- ghci> 2 `compare` 2
-- EQ
-- ghci> 3 `compare` 2
-- GT

-- With lists, numbers, and Boolean values, finding monoids was just a matter of looking at
-- already existing commonly used functions and seeing if they exhibited some sort of monoid behavior.
-- With Ordering, we need to look a bit harder to recognize a monoid.
-- It turns out that the ordering Monoid instance is just as intuitive as the ones we’ve met so far,
-- and it’s also quite useful:

-- instance Monoid Ordering where
--     mempty = EQ
--     LT `mappend` _ = LT
--     EQ `mappend` y = y
--     GT `mappend` _ = GT


-- The instance is set up like this:
-- When we mappend two Ordering values, the one on the left is kept, unless the value on the left is EQ.
-- If the value on the left is EQ, the right one is the result. The identity is EQ.
-- At first, this may seem kind of arbitrary, but it actually resembles the way we alphabetically compare words.
-- We look at the first two letters, and if they differ, we can already decide which word would go first in a dictionary.
-- However, if the first two letters are equal, then we move on to comparing the next pair of letters and repeat the process.

-- It’s important to note that in the Monoid instance for Ordering,
-- x `mappend` y doesn’t equal y `mappend` x.
-- Because the first parameter is kept unless it’s EQ,
-- LT `mappend` GT will result in LT, whereas GT `mappend` LT will result in GT:

-- ghci> LT `mappend` GT
-- LT
-- ghci> GT `mappend` LT
-- GT
-- ghci> mempty `mappend` LT
-- LT
-- ghci> mempty `mappend` GT
-- GT

-- Let’s say we are writing a function that takes two strings, compares their lengths, and returns an Ordering.
-- But if the strings are of the same length, instead of returning EQ right away, we want to compare them alphabetically.

-- Here’s one way to write this:

-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = length x `compare` length y
--                         b = x `compare` y
--                     in  if a == EQ then b else a

-- We name the result of comparing the lengths a and the result of the alphabetical comparison b,
-- and then if the lengths are equal, we return their alphabetical ordering.

-- But by employing our understanding of how Ordering is a monoid, we can rewrite this function in a much simpler manner:

-- import Data.Monoid

-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = (length x `compare` length y) `mappend`
--                     (x `compare` y)



----------------------------
-- Maybe the Monoid
----------------------------

-- Let’s take a look at the various ways that Maybe a can be made an instance of Monoid and how those instances are useful.
-- One way is to treat Maybe a as a monoid only if its type parameter a is a monoid
-- as well and then implement mappend in such a way that it uses the mappend operation
-- of the values that are wrapped with Just. We use Nothing as the identity, and so if one
-- of the two values that we’re mappending is Nothing

-- Here’s the instance declaration:

-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing
--     Nothing `mappend` m = m
--     m `mappend` Nothing = m
--     Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- Notice the class constraint.
-- It says that Maybe a is an instance of Monoid only if a is an instance of Monoid.
-- If we mappend something with a Nothing, the result is that something.
-- If we mappend two Just values, the contents of the Justs are mappended and then wrapped back in a Just.
-- We can do this because the class constraint ensures that the type of what’s inside the Just is an instance of Monoid.

-- ghci> Nothing `mappend` Just "andy"
-- Just "andy"
-- ghci> Just LT `mappend` Nothing
-- Just LT
-- ghci> Just (Sum 3) `mappend` Just (Sum 4)
-- Just (Sum {getSum = 7})

-- This is useful when we’re dealing with monoids as results of computations that may have failed.
-- Because of this instance, we don’t need to check if the computations have failed
-- by seeing if they’re a Nothing or Just value; we can just continue to treat them as normal monoids.


-- But what if the type of the contents of the Maybe is not an instance of Monoid?
-- Well, one thing we can do is discard the second value and keep the first one.
-- For this purpose, the First a type exists. Here’s its definition:

-- newtype First a = First { getFirst :: Maybe a }
--     deriving (Eq, Ord, Read, Show)

-- We take a Maybe a and wrap it with a newtype. The Monoid instance is as follows:
-- instance Monoid (First a) where
--     mempty = First Nothing
--     First (Just x) `mappend` _ = First (Just x)
--     First Nothing `mappend` x = x

-- mempty is just a Nothing wrapped with the First newtype constructor.
-- If mappend’s first parameter is a Just value, we ignore the second one.
-- If the first one is a Nothing, then we present the second parameter as a result,
-- regardless of whether it’s a Just or a Nothing:

-- ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
-- Just 'a'
-- ghci> getFirst $ First Nothing `mappend` First (Just 'b')
-- Just 'b'
-- ghci> getFirst $ First (Just 'a') `mappend` First Nothing
-- Just 'a'

-- First is useful when we have a bunch of Maybe values and we just want to know if any of them is a Just.
-- The mconcat function comes in handy:

-- ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- Just 9


-- If we want a monoid on Maybe a such that the second parameter is kept
-- if both parameters of mappend are Just values, Data.Monoid provides the Last a type,
-- which works like First a, but the last non-Nothing value is kept when mappending and using mconcat:

-- ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
-- Just 10
-- ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")
-- Just "two"



----------------------------
--Folding with Monoids
----------------------------

-- One of the more interesting ways to put monoids to work is to
-- have them help us define folds over various data structures.
-- Lists aren’t the only data structure that can be folded over.
-- We can define folds over almost any data structure. Trees especially lend themselves well to folding.

-- Because there are so many data structures that work nicely with folds,
-- the Foldable type class was introduced.
-- Much like Functor is for things that can be mapped over,
-- Foldable is for things that can be folded up!
-- It can be found in Data.Foldable,
-- and because it exports functions whose names clash with the ones from the Prelude,
-- it’s best imported qualified:

-- import qualified Data.Foldable as F

-- Let’s compare the types of Foldable’s foldr and foldr from Prelude to see how they differ:
-- ghci> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- ghci> :t F.foldr
-- F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
