maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n - 1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [b] = []
zip' [a] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs

-- fibinacci sequence
fib = 1:1:[ a + b | (a, b) <- zip fib (tail fib)]


-- Quick sort Algorithm

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      greaterThan    = [a | a <- xs, a > x]
  in quickSort smallerOrEqual ++ [x] ++ quickSort greaterThan


-- recursion using folds
-- A fold takes a binary function (one that takes two parameters, such as + or div),
-- a starting value (often called the accumulator), and a list to fold up.
-- Lists can be folded up from the left or from the right.

-- Exmple using left fold
sum' :: Num (a) => [a] -> a
sum' xs = foldl (\acc nextVal -> acc + nextVal) 0 xs
-- Simplyfing the example even further
-- sum' xs = foldl (+) 0 xs

-- VS
-- sum' :: Num (a) => [a] -> a
-- sum' [] = 0
-- sum' (x:xs) = x + sum xs

-- Example using right fold
-- The right fold function, foldr, is similar to the left fold just in the oposite way,
-- except the accumulator eats up the values from the right.
-- Also, the order of the parameters in the right fold’s binary function is reversed:
-- The current list value is the first parameter, and the accumulator is the second.
-- (It makes sense that the right fold has the accumulator on the right,
-- since it folds from the right side.)

-- map
map' :: (a -> b) -> [a] -> [b]
map' fn xs = foldr (\x acc -> fn x : acc) [] xs

-- We could have also implemented this function from left to right
-- however the ++ symbol is slower than :
-- so we usually use right folds when we’re building up new lists from a list.
-- e.g.
map2' :: (a -> b) -> [a] -> [b]
map2' fn xs = foldr (\x acc -> acc ++ [fn x]) [] xs

-- One big difference between the two types of folds
-- is that right folds work on infinite lists, whereas left ones don’t!

-- This is an example where we use a boolean as the accumulator
-- We start by assuming that the needle is not in the haystack then we
-- fold each element inside the list checking if the element we are on is the needle
elem2' :: (Eq a) => a -> [a] -> Bool
elem2' x xs = foldr (\y acc -> if y == x then True else acc) False xs

-- foldl1 and foldr1
-- these are basically the same as their counterparts however the don't require you to
-- pass a accumulator the simply assume that the first element in the list respective of direction is the
-- accumulator

-- We start at the beginning of the list and then compare each element
-- with the accumulator. If it’s greater than our accumulator,
-- we keep it as the new accumulator; otherwise, we keep the old one.
maximum2' :: (Ord a) => [a] -> a
-- maximum2' xs = foldr1 max xs
maximum2' = foldr1 max

-- Implementing reverse with foldr
reverse2' :: [a] -> [a]
-- reverse2' xs = foldl (\acc x -> x : acc) [] xs
-- reverse2' = foldl (\acc x -> x : acc) []

-- The function \acc x -> x : acc is just like the : function,
-- except that the parameters are flipped.
reverse2' = foldl (flip (:)) []


product2' :: Num a => [a] -> a
product2' = foldl (*) 1

filter2' :: (a -> Bool) -> [a] -> [a]
filter2' fn = foldr (\x acc -> if fn x then x:acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' = foldr (&&) True


-- Scans are used to monitor the progress of a function that can be implemented
-- as a fold.
-- scanl and scanr are the same as foldl and foldr excecpt they report on all intermediate
-- accumlators

-- scanl (+) 0 [3,5,2,1]
-- ==> [0, 3, 8, 10, 11] //All the intermidiate values have been returned
--  vs foldl
-- foldl (+) 0 [3,5,2,1]
-- ==> 11

-- scanl (flip (:)) [] [3,2,1]
-- ==> [[],[3],[2,3],[1,2,3]]

-- scanr will place the result in the head of the list.
-- scanr (+) 0 [3,5,2,1]
-- ==> [11,8,3,1,0]

--  How many elements does it take for the sum of the square roots of all
-- natural numbers to exceed 1,000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- => 131
-- sum (map sqrt [1..131])
-- => 1005.0942035344083
-- sum (map sqrt [1..130])
-- => 993.6486803921487

-- > 1 and only divisible by itself
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime num = primeTest num 2
  where
    primeTest :: Int -> Int -> Bool
    primeTest num x
      | num == x = True
      | num `mod` x == 0 = False
      | otherwise = primeTest num (x + 1)



primesLessThan :: Int -> [Int]
primesLessThan 1 = []
primesLessThan n
  | isPrime nextVal = nextVal : primesLessThan nextVal
  | otherwise = primesLessThan nextVal
    where nextVal = n - 1

-- using generators

primesLessThan' :: Int -> [Int]
primesLessThan' n = [x | x <- [2..n], x < n && isPrime x]
