multBy3 :: Int -> Int -> Int -> Int
-- Each argument returns a function
-- multBy3 :: Int -> (Int -> (Int -> Int))
multBy3 x y z = x * y * z

-- Using function currying
multTwoWithNine = multBy3 9

-- usage multTwoWithNine 2 3

-- Non high-order implentation
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x


-- Partial application implentation
compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- patially applying infix operators
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- usage
-- divideByTen 200 -> 20.0
-- This is the same as
-- 200 / 10
-- (/10) 200

isUpperAlphanum :: (Char -> Bool)
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a
applyTwice fn x = fn(fn x)

-- example usage
-- applyTwice (+3) 10
-- applyTwice (++ " ") "HEY NOW"
-- applyTwice (3:) [1]

--zipWith'
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' fn (x:xs) (y:ys) = fn x y : zipWith' fn xs ys


--flip'
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g y x = f x y

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 fn x y = fn y x

-- A usefull example of using lambdas
flip3 :: (a -> b -> c) -> b -> a -> c
flip3 fn = \x y -> fn y x
-- as opesed to
-- flip fn x y = fn x y
-- example usage
-- zipWith (flip3 (++)) ["love you", "love me"] ["i ", "you "]
-- map (flip subtract 20) [1,2,3,4,5]


-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' fn (x:xs) = fn x : map' fn xs

-- usage
-- map' ( * 2) [1..10]
-- => [2,4,6,8,10,12,14,16,18,20]

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' fn (x:xs)
  | fn x = x : filter' fn xs
  | otherwise = filter' fn xs

-- usage
-- filter' ( > 3) [1..20]

-- example of compising all
-- isEven :: Integral a => a -> Bool
-- isEven a = a `mod` 2 == 0
-- filter' (isEven) (map' (*3) [1..20])

-- quickSort highorder Style
-- Much more readable then the list comprehension example
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      greaterThan = filter (> x) xs
  in quickSort smallerOrEqual ++ [x] ++ quickSort greaterThan


-- Largest divisible
largestDivisible :: Int
largestDivisible = head (filter fn [100000,99999..])
  where fn x = x `mod` 3829 == 0

-- sum of all odd squares that are less than 10,000
-- sum (takeWhile (< 10000) (filter odd (map (^ 2) [1..])))
-- the same could have been achieved with a list comprehension
-- sum (takeWhile (< 10000) [x ^ 2 | x <- [1..], odd x ])

-- Collatz sequences.
-- Start with any natural number.
-- If the number is 1, stop.
-- If the number is even, divide it by 2.
-- If the number is odd, multiply it by 3 and add 1.
-- Repeat the algorithm with the resulting number.

chain :: Int -> [Int]
chain 1 = [1]
chain n
  | odd n = n : chain ((n * 3) + 1)
  | even n = n : chain (n `div` 2)

-- Here is the problem we want to solve:
-- For all starting numbers between 1 and 100,
-- how many Collatz chains have a length greater than 15?

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- implementing numLongChains with a lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


-- zipWith (\a b  -> (a * 30 + 3) / b) [1,3,4,5] [1,3,4,5]
-- example with pattern matching
-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- this is equivalent to the one above because of the way functional currying works
-- this example is clearly less readable then the version above therefore is typically used
addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z
