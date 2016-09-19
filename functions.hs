-- Where clause
import Geometry

calcBMI :: [(Double, Double)] -> [Double]
calcBMI xs = [bmi weight height | (weight, height) <- xs ]
  where bmi weight height = weight / height ^ 2


-- Let clause
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- let in list comprensions
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-- the (w, h) <- xs is known as the generator

-- Case clause
-- syntax
 -- case expression of pattern -> result
 --                    pattern -> result
 --                    pattern -> result
 --                    ...

-- example
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty!"
                                               [x] -> "a sigleton list."
                                               xs -> "a longer list."

-- refactored version using the where clause
describeList' :: [a] -> String
describeList' xs = "The list is a " ++ what xs
  where what [] = "empty"
        what [a] = "a sigleton list."
        what xs = "a longer list."
