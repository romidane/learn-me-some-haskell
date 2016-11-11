import qualified Data.Map as Map

-- Let’s say that a shape can be a circle or a rectangle

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- The Circle value constructor has three fields, which take floats
-- The Rectangle value constructor has four fields that accept floats.
-- Value constructors are actually functions that ultimately return a value of a data type

-- ghci> :t Circle
-- Circle :: Float -> Float -> Float -> Shape
-- ghci> :t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape

-- area :: Shape -> Float
-- area (Circle _ _ r) = pi * r ^ 2
-- area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Usage
-- area $ Circle 10 20 10
-- => 314.15927
-- area $ Rectangle 0 0 100 100

-- So if we tried to to simple call (Circle 10 20 10) as it is the compiler would
-- complain because currently the haskell compiler doesn't know how to compile it
-- We need to tell it how to print it to the console. Remember that when we try
-- to print a value out from the prompt, Haskell first applies the show function
-- to it to get the string representation of our value, and then it prints that
-- to the terminal.

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  -- deriving (Show)

-- Value constructors are functions, so we can map them, partially apply them,
-- and so on. If we want a list of concentric circles with different radii,
-- we can do this:

-- map (Circle 20 10) [4,5,6]
-- => [Circle 10.0 20.0 4.0, Circle 10.0 20.0 5.0, Circle 10.0 20.0 6.0]


-- type constructor and the value constructor
-- data X            =  XY


-- Our data type is good, but it could be better.
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

-- Usage
-- area (Rectangle (Point 0 0) (Point 10 10))
-- area (Circle (Point 0 0) 24)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (x+b)) r;
nudge (Rectangle (Point x1 y1) (Point y2 x2)) a b
  = (Rectangle (Point (x1 + a) (y1 + a)) (Point (x2 + a) (y2 + a)) )

-- Usage
-- nudge (Circle (Point 34 34) 10) 5 10
-- Circle (Point 39.0 44.0) 10.0

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- usage
-- nudge (baseRect 40 100) 60 23
-- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)


-- You can also export your data types in your custom modules. To do that,
-- just write your type along with the functions you are exporting, and then add
-- some parentheses that specify the value constructors that you want to export,
-- separated by commas. If you want to export all the value constructors for a
-- given type, just write two dots (..).

-- module Shapes
-- ( Point(..),
--   Shape(..),
--   area,
--   nudge,
--   baseRect,
--   baseCircle
-- ) where

-- It’s the same as writing Shape (Rectangle, Circle), but shorter.


-- ============================
-- Record Syntax
-- ============================

-- Example User
-- The information that we want to store about that person is first name,
-- last name, age, height, phone number, and favorite ice cream flavor.
-- data Person = Person String String Int Float String String deriving (Show)

-- guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- create functions to get specific pieces of information about a person
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

-- Certainly there must be a better way! this was not fun at all!

data Person = Person {
  firstName   :: String,
  lastName    :: String,
  age         :: Int,
  height      :: Float,
  phoneNumber :: String,
  flavor      :: String
} deriving (Show)

-- By using record syntax to create this data type, Haskell automatically makes
-- these functions: firstName, lastName, age, height, phoneNumber, and flavor.

-- more examples
-- data Car = Car String String Int deriving (Show)
-- fm = Car "Ford" "Mustang" 1978

data Car = Car {
  make  :: String,
  model :: String,
  year  :: Int
} deriving (Show)

fm = Car { make = "Ford", model = "Mustang", year = 1978 }




-- ============================
-- Type Parameters
-- ============================

-- Type constructors can take types as parameters to produce new types
-- data Maybe a = Nothing | Just a

-- Type parameters are useful because they allow us to make data types that can
-- hold different things. For instance, we could make a separate Maybe-like
-- data type for every type that it could contain, like so:

data IntMaybe = INothing | IJust Int

data StringMaybe = SNothing | SJust String

data ShapeMaybe = ShNothing | ShJust Shape


-- Should We Parameterize Our Car?
data Car' a b c = Car' {
  company' :: a,
  model'   :: b,
  year'    :: c
} deriving (Show)

tellCar :: (Show a) => Car' String String a -> String
tellCar Car' {company' = c, model' = m, year' = y} =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- => "This Ford Mustang was made in 1967"

-- Vector von Doom

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + m*j + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)






-- ============================
-- Derived Instances
-- ============================

-- Consider the an example where 2 people can't have the same details

data Person' = Person' {
  firstName' :: String,
  lastName'  :: String,
  age'       :: Int
} deriving (Eq, Show, Read)

mikeD = Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
adRock = Person' {firstName' = "Adam", lastName' = "Horovitz", age' = 41}
mca = Person' {firstName' = "Adam", lastName' = "Yauch", age' = 44}

-- Usage
-- mca == adRock
-- => True

-- mikeD == mikeD
--  => True

-- mikeD == Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
-- => True

mysteryDude = "Person' { firstName' =\"Michael\"" ++
                     ", lastName' =\"Diamond\"" ++
                     ", age' = 43}"

-- read mysteryDude :: Person
-- => Person {firstName = "Michael", lastName = "Diamond", age = 43}

-- read mysteryDude == mikeD
-- => True




-- ============================
-- Any Day of the Week
-- ============================

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Because all the type’s value constructors are nullary (that is, they don’t have any fields),
-- we can make it part of the Enum type class.
-- The Enum type class is for things that have predecessors and successors.

-- We can also make it part of the Bounded type class,
-- which is for things that have a lowest possible value and highest possible value.

-- == usage ==

-- Because it’s part of the Show and Read type classes,
-- we can convert values of this type to and from strings.

-- show Wednesday
-- => "Wednesday"
-- read "Saturday" :: Day
-- => Saturday

-- Because it’s part of the Eq and Ord type classes, we can compare or equate days.
-- Saturday == Sunday
-- False
-- Saturday == Saturday
-- True
-- Saturday > Friday
-- True
-- Monday `compare` Wednesday
-- LT

-- It’s also part of Bounded, so we can get the lowest and highest day.

-- minBound :: Day
-- Monday
-- maxBound :: Day
-- Sunday

-- As it’s an instance of Enum, we can get predecessors and successors
-- of days and make list ranges from them!

-- succ Monday
-- Tuesday
-- pred Saturday
-- Friday
-- [Thursday .. Sunday]
-- [Thursday,Friday,Saturday,Sunday]
-- [minBound .. maxBound] :: [Day]
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

-- ============================
-- Type Synonyms
-- ============================
-- [Char] and String types are equivalent and interchangeable.
-- That’s implemented with type synonyms.

-- Here’s how the standard library defines String as a synonym for [Char]:
-- type String = [Char]

-- The type keyword here might be misleading, because a new type is not being created
-- (that’s done with the data keyword). Rather, this defines a synonym for an existing type.

-- Haskell programmers give type synonyms to the String type when they want to
-- convey more information about the strings in their functions—what they actually represent.

-- ============================
-- Making Our Phonebook Prettier
-- ============================


-- type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- phoneBook :: [(String, String)]
phoneBook :: PhoneBook
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber   -> PhoneBook -> Bool
-- vs
-- inPhoneBook :: String -> String -> [(String, String)] -> Bool
inPhoneBook name pnumber phoneBook = (name, pnumber) `elem` phoneBook


-- ============================
-- Parameterizing Type Synonyms
-- ============================

type AssocList k v = [(k, v)]

-- exampleFnc :: (Eq k) => k -> AssocList k v -> Maybe v
-- exampleFnc

-- We can partially apply functions to get new functions, we can partially apply
-- type parameters and get new type constructors from them.

-- import qualified Data.Map as Map
-- type IntMap v = Map Int v
-- or
-- type IntMap = Map Int

-- ============================
-- Important Note about type synonyms!!!
-- ============================
 -- IntMap or AssocList doesn’t mean that we can do stuff like
 -- `AssocList [(1,2), (4,5),(7,9)]`
 -- All it means is that we can refer to its type by using different names.
 -- We can do
 -- `[(1,2),(3,5),(8,9)] :: AssocList Int Int`
 -- which will make the numbers inside assume a type of Int.




-- ============================
-- Go Left, Then Right
-- ============================

-- When we are not intersed in why a function failed then we use the `Maybe` data type
-- But when we are intersted in why something went wrong we use the `Either` data type

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing            -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- usage
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]


-- lockerLookup 101 lockers
-- => Right "JAH3I"
-- lockerLookup 100 lockers
-- => Left "Locker 100 is already taken!"
-- lockerLookup 102 lockers
-- => Left "Locker number 102 doesn't exist!"
-- lockerLookup 110 lockers
-- => Left "Locker 110 is already taken!"
-- lockerLookup 105 lockers
-- => Right "QOTSA"


-- ============================
-- Recursive Data Structures
-- ============================

-- example
-- A list can be an empty list, or it can be an element joined together
-- with a : with another list (that might be an empty list).
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- Record syntax
-- data List a = Empty | Cons { listHead :: a, listTail :: List a}
--     deriving (Show, Read, Eq, Ord)

-- Examples
-- 5 `Cons` Empty
-- Cons 5 Empty
-- 3 `Cons` (4 `Cons` (5 `Cons` Empty))
-- Cons 3 (Cons 4 (Cons 5 Empty))
-- We called our Cons constructor in an infix manner so you can see how it’s
-- just like :. Empty is like [], and 4 `Cons` (5 `Cons` Empty) is like 4:(5:[]).

-- Improving our list


-- We can define functions to be automatically infix by naming them using only
-- special characters. We can also do the same with constructors, since they’re
-- just functions that return a data type. There is one restriction
-- however: Infix constructors must begin with a colon.

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- First, notice a new syntactic construct: the fixity declaration, which is the
-- line above our data declaration. When we define functions as operators,
-- we can use that to give them a fixity (but we don’t have to).
-- A fixity states how tightly the operator binds and whether it’s left-associative or right-associative.
-- For instance, the * operator’s fixity is infixl 7 *, and the + operator’s fixity is infixl 6.
-- That means that they’re both left-associative (in other words, 4 * 3 * 2 is the
-- same as (4 * 3) * 2), but * binds tighter than +, because it has a greater fixity.
-- So 5 * 4 + 3 is equivalent to (5 * 4) + 3.

-- Examples
-- 3 :-: 4 :-: 5 :-: Empty
-- 3 :-: (4 :-: (5 :-: Empty))
-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty
-- ghci> 100 :-: a
-- 100 :-: (3 :-: (4 :-: (5 :-: Empty)))

-- This is how ++ is defined for normal lists:
-- infixr 5  ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

--Implementing our own version of this.
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
-- Notice how we pattern matched on (x :-: xs). That works because
-- pattern matching is actually about matching constructors.
-- We can match on :-: because it is a constructor for our own list type,
-- and we can also match on : because it is a constructor for the built-in list type.
-- The same goes for []. Because pattern matching works (only) on constructors,
-- we can match for normal prefix constructors or stuff like 8 or 'a', which are
-- basically constructors for the numeric and character types, respectively.



-- Example usage
-- let a = 3 :-: 4 :-: 5 :-: []
-- let b = 6 :-: 7 :-: 8 :-: []
-- > a ^++ b
-- -> 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))


-- ============================
-- Making your own type classes
-- ============================

-- Inside the Eq type class
-- It defines the functions == and /=
-- If we have the type Car and comparing two cars with the equality function == makes sense,
-- then it makes sense for Car to be an instance of Eq.

-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)


-- class Eq a where means a new type class called Eq is being defined.
-- The a is the type variable, so a will play the role of the type that
-- will soon be made an instance of Eq.

-- Next, several functions are defined. Note that it’s not mandatory to implement
-- the function bodies themselves; just their type declarations are required.

-- So once we have a class, what can we do with it? We can make type instances of
-- that class and get some nice functionality.

data TraficLight = Red | Yellow | Green

-- It defines the states of a traffic light. Notice how we didn’t derive any class instances for it.
-- That’s because we’re going to write some instances by hand. Here’s how we make it an instance of Eq:

instance Eq TraficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- We did it by using the instance keyword. So class is for defining new type classes,
-- and instance is for making our types instances of type classes.

-- When we were defining Eq, we wrote class Eq a where,
-- and we said that a plays the role of whichever type
-- will be made an instance later. We can see that clearly here,
-- because when we’re making an instance, we write instance Eq TrafficLight where.
-- We replace the a with the actual type.

-- Because == was defined in terms of /= and vice versa in the class declaration,
-- we needed to overwrite only one of them in the instance declaration.
-- That’s called the minimal complete definition for the type class—the minimum of
-- functions that we must implement so that our type can behave as the class advertises.
-- To fulfill the minimal complete definition for Eq, we need to overwrite either == or /=.

-- You can see that we implemented == simply by doing pattern matching.
-- Since there are many more cases where two lights aren’t equal,
-- we specified the ones that are equal, and then just did a catchall pattern saying
-- that if it’s none of the previous combinations, then two lights aren’t equal.

-- Let’s make this an instance of Show by hand, too. To satisfy the minimal
-- complete definition for Show, we just need to implement its show function,
-- which takes a value and turns it into a string:

instance Show TraficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

-- instance Eq Maybe where
-- The a must be a concrete type, and Maybe is not; it’s a type constructor that
-- takes one parameter and then produces a concrete type.

--------------
-- Subclasing
--------------
-- class (Eq a) => Nums a where
--   ...

-- There are a lot of places where we can cram in class constraint
-- That’s all there is to subclassing—it’s just a class constraint on a
-- class declaration! When defining function bodies in the class declaration or
-- in instance declarations, we can assume that a is a part of Eq,
-- so we can use == on values of that type.

--------------------------------------------------
-- Parameterized Types As Instances of Type Classes
---------------------------------------------------
-- What makes Maybe different from, say, TrafficLight is that Maybe in itself
-- isn’t a concrete type—it’s a type constructor that takes
-- one type parameter (like Char) to produce a concrete type (like Maybe Char)

-- It would also be tedious if we needed to make a separate instance for
-- every possible type that Maybe’s type parameter could take on.
-- If we needed to write instance Eq (Maybe Int) where, instance Eq (Maybe Char) where,
-- and so on for every type, we would get nowhere. That’s why we can just leave
-- the parameter as a type variable, like so:

-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False

-- Take into account that the type you’re trying to make an instance of will
-- replace the parameter in the class declaration. The a from class Eq a where
-- will be replaced with a real type when you make an instance, so try to mentally
-- put your type into the function type declarations as well. The following type
-- declaration really doesn’t make much sense:

-- (==) :: Maybe -> Maybe -> Bool

-- But this does:

-- (==) :: (Eq m) => Maybe m -> Maybe m -> Bool

-- This is just something to think about, because == will always have a
-- type of (==) :: (Eq a) => a -> a -> Bool, no matter what instances we make.

-- If you want to see what the instances of a type class are, just
-- type :info YourTypeClass in GHCi. For instance, typing :info Num



--------------------------------------------------
-- A Yes-No Type Class
--------------------------------------------------

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

-- But what’s id? It’s just a standard library function that takes a parameter
-- and returns the same thing, which is what we would be writing here anyway.

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

-- We didn’t need a class constraint, because we made no assumptions about the
-- contents of the Maybe. We just said that it’s true-ish if it’s a Just value
-- and false-ish if it’s a Nothing. We still need to write out (Maybe a) instead
-- of just Maybe. If you think about it, a Maybe -> Bool function can’t exist
-- (because Maybe isn’t a concrete type), whereas a Maybe a -> Bool is fine and dandy.


-- Now let’s make a function that mimics the if statement, but that works with YesNo values.
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
  then yesResult
  else noResult

-- This takes a YesNo value and two values of any type. If the yes-no--ish value
-- is more of a yes, it returns the first of the two values; otherwise,
-- it returns the second of them.

-- yesnoIf [] "YEAH!" "NO!"
-- yesnoIf [2,3,4] "YEAH!" "NO!"
-- yesnoIf True "YEAH!" "NO!"
-- yesnoIf (Just 500) "YEAH!" "NO!"



--------------------------------------------------
-- The Functor Type Class
--------------------------------------------------
-- Functor type class, which is for things that can be mapped over.

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- We see that fmap takes a function from one type to another and a functor value
-- applied with one type and returns a functor value applied with another type.

-- Hmm this seems awfully similar to the map function
-- map :: (a -> b) -> [a] -> [b]
-- It takes a function from one type to another and a list of one type and returns
-- a list of another type.

-- Here’s how the list is an instance of the Functor type class:

-- instance Functor [] where
--   fmap = map

-- Notice how we didn’t write instance Functor [a] where.
-- This is because f must be a type constructor that takes one type
-- [a] is already a concrete type (of a list with any type inside it),
-- while [] is a type constructor that takes one type and can produce types
-- such as [Int], [String], or even [[String]].

-- Since for lists, fmap is just map, we get the same results when using
-- these functions on lists:

-- fmap (*2) [1..3]
-- map (*2) [1..3]

-- Maybe As a Functor
-- Types that can act like a box can be functors. You can think of a list as a box
-- that can be empty or have something inside it, including another box.
-- That box can also be empty or contain something and another box, and so on.
-- So, what else has the properties of being like a box? For one, the Maybe a type.
-- In a way, it’s like a box that can hold nothing (in which case it has the value of Nothing),
-- or it can contain one item (like "HAHA", in which case it has a value of Just "HAHA").

-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing

-- Usage
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
-- => Just "Something serious. HEY GUYS IM INSIDE THE JUST"
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing


-- Either a As a Functor
-- The Either type class takes 2 type parameters.

-- data Either a b = Left a | Right b

-- The Functor type class wants a type constructor that takes only one type parameter
-- So how can we achieve this?.. Partial application of course! By only feeding the first parameter

-- instance Functor (Either a) where
--   fmap f (Right x) = Right (f x)
--   fmap f (Left x) = Left x

-- If fmap were specifically for Either a, the type signature would be this:

-- (b -> c) -> Either a b -> Either a c
-- (b -> c) -> (Either a) b -> (Either a) c

-- The function is mapped in the case of a Right value constructor, but it isn’t
-- mapped in the case of a Left
-- If we wanted to map one function over both of them, a and b would need to be the
-- same type. Think about it: If we try to map a function that takes a string
-- and returns a string, and b is a string but a is a number, it won’t really work out.
