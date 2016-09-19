import Data.List

--------           ------------
--        Problem 1
--------           ------------
-- (*) Find the last element of a list.
--  myLast [1,2,3,4] => 4

myLast :: [a] -> a
myLast = head . foldr (\x acc -> acc ++ [x]) []

myLast2 :: [a] -> a
myLast2 = head . reverse
-- usage myLast [1,2,3,4] => 4

myLast3 :: Foldable t => t c -> c
myLast3 = foldl1 (\acc x -> x)

myLast4 :: [a] -> a
myLast4 [] = error "empty list"
myLast4 [x] = x
myLast4 (_:xs) = myLast3 xs

myLast5 :: [a] -> a
myLast5 [] = error "empty list"
myLast5 x = x !! (length x - 1)

-- or just use last [1,2,3,4]


--------           ------------
--        Problem 2
--------           ------------
-- (*) Find the last but one element of a list.
-- myButLast [1,2,3,4] => 3
-- myButLast ['a'..'z'] => y

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = x
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

myButLast2 :: [a] -> a
myButLast2 [] = error "empty list"
myButLast2 [x] = x
myButLast2 x = x !! (length x - 2)

myButLast3 :: [a] -> a
myButLast3 = head . reverse . init

myButLast4 :: [a] -> a
myButLast4 = last . init

--------           ------------
--        Problem 3
--------           ------------
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- elementAt [1,2,3] 2 => 2

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt xs index
  | index > length xs = error "End of list"
  | index < 1 = error "To far back bro"
  | otherwise = xs !! (index - 1)

elementAt2 :: [a] -> Int -> a
elementAt2 xs index = last $ foldl (\acc x -> if length acc == (index) then acc else acc ++ [x]) [] xs

elementAt3 :: [a] -> Int -> a
elementAt3 xs index = last $ take index xs
