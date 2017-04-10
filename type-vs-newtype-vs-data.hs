-- type vs. newtype vs. data

-----------------------------------------------
-- type
-----------------------------------------------

-- The type keyword is for making type synonyms. We just give another name to an already existing type
-- so that the type is easier to refer to. Say we did the following:

-- type IntList = [Int]

-- All this does is allow us to refer to the [Int] type as IntList. They can be used interchangeably.
-- We don’t get an IntList value constructor or anything like that.
-- Because [Int] and IntList are only two ways to refer to the same type,
-- it doesn’t matter which name we use in our type annotations:

-- ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])
-- [1,2,3,1,2,3]

-- We use type synonyms when we want to make our type signatures more descriptive.
-- We give types names that tell us something about their purpose in the context of the functions where they’re being used.



-----------------------------------------------
-- newtype
-----------------------------------------------

-- The newtype keyword is for taking existing types and wrapping them in new types,
-- mostly so it’s easier to make them instances of certain type classes.
-- When we use newtype to wrap an existing type, the type that we get is separate from the original type.
-- Suppose we make the following newtype:

-- newtype CharList = CharList { getCharList :: [Char] }

-- We can’t use ++ to put together a CharList and a list of type [Char].
-- We can’t even use ++ to put together two CharList lists, because ++ works only on lists, and the CharList type isn’t a list,
-- even though it could be said that CharList contains a list.
-- We can, however, convert two CharLists to lists, ++ them, and then convert that back to a CharList.

-- When we use record syntax in our newtype declarations, we get functions for converting between the new type and the original type—namely the value constructor of our newtype and the function for extracting the value in its field. The new type also isn’t automatically made an instance of the type classes that the original type belongs to, so we need to derive or manually write it.

-- In practice, you can think of newtype declarations as data declarations that can have only one constructor and one field.
-- If you catch yourself writing such a data declaration, consider using newtype.

-- Practical examples of using the newtype

-- newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- ghci> CharList "this will be shown!"
-- CharList {getCharList = "this will be shown!"}
-- ghci> CharList "benny" == CharList "benny"
-- True
-- ghci> CharList "benny" == CharList "oisters"
-- False


-- In this particular newtype, the value constructor has the following type:

-- CharList :: [Char] -> CharList

-- It takes a [Char] value, such as "my sharona" and returns a CharList value.
-- From the preceding examples where we used the CharList value constructor, we see that really is the case.
-- Conversely, the getCharList function, which was generated for us because we used record syntax in our newtype,
-- has this type:

-- getCharList :: CharList -> [Char]

-- It takes a CharList value and converts it to a [Char] value.
-- You can think of this as wrapping and unwrapping,
-- but you can also think of it as converting values from one type to the other.


-- If we wanted to make the tuple an instance of Functor in such a way that when we fmap a function over a tuple,
-- it is applied to the first component of the tuple?
-- That way, doing fmap (+3) (1, 1) would result in (4, 1).
-- To get around this, we can newtype our tuple in such a way that the second type parameter
-- represents the type of the first component in the tuple:

-- newtype Pair b a = Pair { getPair :: (a, b) }

-- ghci> :t Pair
-- Pair :: (a, b) -> Pair b a
-- ghci> :t getPair
-- getPair :: Pair b a -> (a, b)

-- And now we can make it an instance of Functor so that the function is mapped over the first component:

-- instance Functor (Pair c) where
--     fmap f (Pair (x, y)) = Pair (f x, y)

-- As you can see, we can pattern match on types defined with newtype.
-- We pattern match to get the underlying tuple, apply the function f to the first component in the tuple,
-- and then use the Pair value constructor to convert the tuple back to our Pair b a.
-- If we imagine what the type fmap would be if it worked only on our new pairs, it would look like this:

-- fmap :: (a -> b) -> Pair c a -> Pair c b

-- Now if we convert a tuple into a Pair b a, we can use fmap over it,
-- and the function will be mapped over the first component:

-- ghci> getPair $ fmap (*100) (Pair (2, 3))
-- (200,3)
-- ghci> getPair $ fmap reverse (Pair ("london calling", 3))
-- ("gnillac nodnol",3)



-----------------------------------------------
-- data
-----------------------------------------------

-- The data keyword is for making your own data types. You can go hog wild with them.
-- They can have as many constructors and fields as you wish and can be used to implement
-- any algebraic data type—everything from lists and Maybe-like types to trees.



-----------------------------------------------
-- Summary
-----------------------------------------------

-- In summary, use the keywords as follows:

-- If you just want your type signatures to look cleaner and be more descriptive, you probably want type synonyms.

-- If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class,
-- chances are you’re looking for a newtype.

-- If you want to make something completely new, odds are good that you’re looking for the data keyword.
