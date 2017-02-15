import           Data.List

main :: IO()
main = do
  content <- getContents
  let threes = groupOf 3 (map read $ lines content)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concatMap (show . fst) path
      pathTime = sum $ map snd path

  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show pathTime


data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = []
groupOf n xs = take n xs : groupOf n (drop n xs)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = sum $ map snd pathA
      timeB = sum $ map snd pathB
      forwardTimeToA = timeA + a
      crossTimeToA = timeB + b + c
      forwardTimeToB = timeB + b
      crossTimeToB = timeA + a + b
      newPathToA = if forwardTimeToA <= crossTimeToA
                      then (A, a):pathA
                      else (C, c):(B, b):pathB
      newPathToB = if forwardTimeToB <= crossTimeToB
                      then (B, b):pathB
                      else (C, c):(B, b):pathA
  in (newPathToA, newPathToB)


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath
