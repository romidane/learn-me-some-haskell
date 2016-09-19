-- Sorting Algorithms

-- Quick sort Algorithm

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      greaterThan    = filter (> x) xs
  in quickSort smallerOrEqual ++ [x] ++ quickSort greaterThan


-- Merge sort
splitInHalf :: (Ord a) => [a] -> ([a], [a])
splitInHalf [] = ([], [])
splitInHalf xs = (take n xs, drop n xs)
  where n = length xs `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge ys (x:xs)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where (left, right) = splitInHalf xs


-- Bubble Sort











-- Binary Search Tree
-- In a binary search tree, an element points to two elements—one on its left and one on its right.
-- The element to the left is smaller; the element to the right is bigger.
-- Each of those elements can also point to two elements (or one or none).
-- In effect, each element has up to two subtrees.

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

-- The treeInsert function is to insert an element into a tree. Here, we first have the base case as a pattern.
-- If we’ve reached an empty subtree, that means we’re where we want to go, and we insert a singleton tree with our element.
-- If we’re not inserting into an empty tree, then we need to do some checking.
-- First, if the element we’re inserting is equal to the root element, we just return a tree that’s the same.
-- If it’s smaller, we return a tree that has the same root value and the same right subtree,
-- but instead of its left subtree, we put a tree that has our value inserted into it.
-- We do the same if our value is bigger than the root element, but the other way around.

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False;
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- example usage
-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums

-- In this foldr, treeInsert is the folding binary function
-- (it takes a tree and a list element and produces a new tree),
-- and EmptyTree is the starting accumulator. nums, of course,
-- is the list we’re folding over.

-- numsTree
-- => Node 5
--     (Node 3
--         (Node 1 EmptyTree EmptyTree)
--         (Node 4 EmptyTree EmptyTree)
--     )
--     (Node 7
--         (Node 6 EmptyTree EmptyTree)
--         (Node 8 EmptyTree EmptyTree)
--     )

-- Checking for valus
