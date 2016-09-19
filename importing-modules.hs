import Data.List
import Data.Char
import qualified Data.Map as Map


-- Or you can select the necessary modules
-- import Data.List (nub)
-- or Import some modules while hidding some
-- import Data.List hidding (nub) // This useful when some module have methods
-- that share the same name function e.g. map filter
-- Another way of import modules is to use qualified imports
-- import qualified Data.Map
-- Usage: Data.Map.filter
-- You can shorten this by aliasing the import
-- import qualified Data.Map as M
-- Usage: M.filter

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- Counting number of occurence of a word in a string
wordsNum :: String -> [(String, Int)]
wordsNum = map (\ws -> (head ws, length ws)) . group . sort . words

-- Attempt to remplement tails' (success)
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (x:xs) = (x:xs) : tails' (tail (x:xs))

-- Needle in haystack
-- any take a predicate and a List
-- isPrefixOf acts as the predicate here i.e (< 5)
-- notice how we can also declare function in the infix format!
-- this method already exists in haskell and is known as isInfixOf
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)


-- Caesar Cipher using the Data.Char module
-- ord 'a'
-- => 97 // ord 'a' returns 97 because 'a' is the ninety-seventh character in the Unicode table of characters.
-- chr 97
-- => 'a'

-- map ord "abcdefgh"
-- [97,98,99,100,101,102,103,104]

encode :: Int -> String -> String
-- encode offset msg = map (\c -> chr $ ord c + offset) msg
encode offset msg = map (chr . (+ offset) . ord) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
-- digitSum = foldl (\acc x -> digitToInt x + acc) 0
digitSum = sum . map digitToInt . show

-- is40 :: String
-- is40 = head $ filter (\x -> digitSum x == 40) (map show [1..])
-- A value with a type of Maybe a is sort of like a list of type [a]. Whereas a
-- list can have zero, one, or many elements, a Maybe a typed value can have
-- either zero elements or just one element.

-- find :: (a -> Bool) -> [a] -> Maybe a
-- If find finds an element that satisfies the predicate, it will return that
-- element wrapped in a Just. If it doesn’t, it will return a Nothing
firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

-- Making the function generic
-- lowestBound :: RealFrac a => a -> Int
lowestBound :: Int -> Int
-- lowestBound n = read $ foldl (\acc _ -> acc ++ "9") "" [1..(floor . fromIntegral $ n `div` 9)] :: Int
lowestBound n = read $ replicate (floor . fromIntegral $ (n `div` 9)) '9' :: Int


firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

firstTo2 :: Int -> Maybe Int
firstTo2 n = find (\x -> digitSum x == n) [(lowestBound n)..]



-- Finding keys in linked lists
phoneBook =
  [
    ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- This simple implemantation will find keys only if the exist and will error
-- when don't
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\x -> fst x == key) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing xs

-- This proccess is not the fastest as we have to loop through all the items
-- before we get a find a a match. A better was to do this is to use a linked List
-- the module Data.Map is here to help!

-- turn an association list into a map by using the fromList function from Data.Map.
-- fromList takes an association list (in the form of a list) and returns a map with
-- the same associations.

-- Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v

-- Map.fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
-- => fromList [(3,"shoes"),(4,"trees"),(9,"bees")]

-- If there are duplicate keys in the original association list,
-- the duplicates are just discarded:
-- Map.fromList [("MS",1),("MS",2),("MS",3)]
-- => fromList [("MS",3)]

-- Reimplementing findKey with mapped List
phoneBook' :: Map.Map String String
phoneBook' = Map.fromList $
  [
    ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- Map.lookup takes a key and a map, and tries to find the corresponding value in the map.
-- If it succeeds, it returns the value wrapped in a Just; otherwise, it returns a Nothing:
-- Map.lookup :: (Ord k) => k -> Map.Map k a -> Maybe a
-- Map.lookup "betty" phoneBook
-- => Just "555-2938"
-- Map.lookup "grace" phoneBook
-- => Nothing

-- Map.insert takes a key, a value, and a map, and returns a new map that’s
-- just like the old one, but with the key and value inserted
-- Map.insert :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
-- Map.insert "grace" "341-9021" phoneBook

-- Map.size function from Data.Map, which takes a map and returns its size.
-- Map.size :: Map.Map k a -> Int

-- So suppose we want to convert all the numbers we have into a list of number
-- instead of a string ("123-1232")

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
-- string2digits "948-9282"
-- => [9,4,8,9,2,8,2]

-- Map.map works just like the map form the prelude module expect it maps over
-- a mapped list (Map.Map)

-- Map.map :: (a -> b) -> Map.Map k a -> Map.Map k b


intBook :: Map.Map String [Int]
intBook = Map.map string2digits phoneBook'

-- Map.lookup "betty" intBook
-- => Just [5,5,5,2,9,3,8]

-- Let’s extend our phone book. Say that a person can have several numbers,
-- and we have an association list set up like this:
phoneBook2 =
  [
    ("betty", "555-2938"),
    ("betty", "342-2492"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("patsy", "943-2929"),
    ("patsy", "827-9162"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492"),
    ("penny", "555-2111")
  ]
-- If we just use Map.fromList to put it into a map we will lose some numbers!
-- we’ll use another function found in Data.Map: fromListWith
-- This function acts like fromList, but instead of discarding duplicate keys,
-- it uses a function supplied to it to decide what to do with them.

-- Map.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map.Map k a

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord key) => [(key, value)] -> Map.Map key [value]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

-- Pretty neat
-- phoneBookToMap' phoneBook2
-- fromList [("betty",["342-2492","555-2938"]),("bonnie",["452-2928"]),("lucille",["205-2928"]),("patsy",["827-9162","943-2929","493-2928"]),("penny",["555-2111","853-2492"]),("wendy",["939-8282"])]

-- Map.lookup "patsy" $ phoneBookToMap phoneBook
-- => "827-9162, 943-2929, 493-2928"


-- Now suppose we’re making a map from an association list of numbers, and when a
-- duplicate key is found, we want the biggest value for the key to be kept.
-- We can do that like so:
listOfNums :: (Ord k) => [(k, Int)] -> Map.Map k Int
listOfNums = Map.fromListWith (max)

-- listOfNums [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
-- fromList [(2,100),(3,29),(4,22)]
