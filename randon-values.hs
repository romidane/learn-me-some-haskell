import           System.Random


-- Haskell is a purely functional language. That means it has referential transparency.
-- And that means a function, if given the same parameters twice, must produce the same result twice.
-- That’s really cool, because it allows us to reason about programs, and it enables us to defer evaluation until we really need it.

-- System.Random module has all the functions that satisfy our need for randomness
-- random' :: (RandomGen g, Random a) => g -> (a, g)

-- The RandomGen type class is for types that can act as sources of randomness.
-- The Random type class is for types whose values can be random
-- We can generate random Boolean values by randomly producing either True or False.
-- We can also generate numbers that are random.

-- If we try to translate the type declaration of random to English, we get something like this:
-- It takes a random generator (that’s our source of randomness)
-- and returns a random value and a new random generator.

-- To use our random function, we need to get our hands on one of those random generators.

-- The System.Random module exports a cool type, namely StdGen, which is an instance of the RandomGen type class.
-- We can make a StdGen manually, or we can tell the system to give us one based on a multitude of (sort of) random stuff.

-- To manually make a random generator, use the mkStdGen function.
-- It has a type of mkStdGen :: Int -> StdGen
--  It takes an integer, and based on that, gives us a random generator
-- The random function can return a value of any type that’s part of the Random type class,
-- so we need to inform Haskell which type we want.

someRandomVal = random (mkStdGen 100) :: (Int, StdGen)
--  => (-1352021624,651872571 1655838864)

--  The first component of the tuple is our number,
-- and the second component is a textual representation of our new random generator.

-- If we were to run the randon function with the same generator and the same value we would always get the same
-- "random value"

--  We can use the type annotation to get different types back from that function.

-- random (mkStdGen 949488) :: (Float, StdGen)
-- => (0.8938442,1597344447 1655838864)

-- random (mkStdGen 949488) :: (Bool, StdGen)
-- => (False,1485632275 40692)


-- Generating infinite list of random numbers
-- System.Random comes with a very handy functions to do that

--Below is a reimplentation for of the randoms function
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
    let (value, newGen) = random gen
        in value : randoms' newGen

-- This is a recursive definition. We get a random value and a new generator from the current generator,
-- and then make a list that has the value as its head and random numbers based on the new generator as its tail.
-- Because we need to be able to potentially generate an infinite amount of numbers,
-- we can’t give the new random generator back.

-- Usage
exampleInt = take 5 $ randoms'(mkStdGen 11) :: [Int]
exampleBool = take 5 $ randoms'(mkStdGen 11) :: [Bool]
exampleFloat = take 5 $ randoms'(mkStdGen 11) :: [Float]



finiteRandoms :: (Num n, Eq n, RandomGen g, Random a) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in (value : restOfList, finalGen)

-- Again, this is a recursive definition. We say that if we want zero numbers,
-- we just return an empty list and the generator that was given to us. For any other number of random values,
-- we first get one random number and a new generator. That will be the head.
-- Then we say that the tail will be n - 1 numbers generated with the new generator.
-- Then we return the head and the rest of the list joined and the final generator
-- that we got from getting the n - 1 random numbers.

-- Usage
-- finiteRandoms 3 (mkStdGen 2)
-- ([-2493429425036973456,-4776227652709330959,4748426603123535319],1288005460 1422611300)


-- What if we want a random value in some sort of range? Well, we use randomR for that purpose. It has this type:

-- Here is the signiture for RandomR
-- randomR' :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
                                        -- ((a, a) is the range)
-- Usage
-- randomR (1,6) (mkStdGen 359353)
-- (6,1494289578 40692)

-- randomR (1,6) (mkStdGen 35935335)
-- (3,1250031057 40692)

-- There’s also randomRs, which produces a stream of random values within our defined ranges.
-- ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
-- "ndkxbvmomg"



