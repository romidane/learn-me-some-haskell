-- Function Application with $
-- Most of the time, it’s a convenience function that lets us write fewer parentheses.
--e.g
-- sum (map sqrt [1..10])
-- vs
-- sum $ map sqrt [1..10]
-- When a $ is encountered, the expression on its right
-- is applied as the parameter to the function on its left.

-- more examples
-- sum (filter (> 10) (map (*2) [2..10]))
-- vs
-- sum $ filter (> 10) $ map (*2) [2..10]

-- Another intersting way to use function applications is to use them as function
-- map ($ 3) [(4 +), (10*), (^2), sqrt]
-- => [7.0,30.0,9.0,1.7320508075688772]
-- Here, the function ($ 3) gets mapped over the list.
-- If you think about what the ($ 3) function does, you’ll see that it takes
-- a function and then applies that function to 3. So every function in the list
-- gets applied to 3, which is evident in the result.

-- Function Composition
-- In mathematics, function composition is defined like this: (f º g)(x) = f(g(x)).
-- This means that composing two functions is the equivalent of
-- calling one function with some value and then
-- calling another function with the result of the first function.

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- One use for function composition is making functions on the fly to pass to other functions.
-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]

-- using function composition
-- map (negate . abs ) [5,-3,-6,7,-3,2,-19,24]

-- Another example
-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- vs --
-- map (\xs -> negate $ sum $ tail xs) [[1..5],[3..6],[1..7]] //A slight improvement
-- vs --
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]] // Winner So much cleaner!!


-- Function Composition with Multiple Parameters
-- if we want to use them in function composition, we usually must partially apply
-- them so that each function takes just one parameter.

-- sum (replicate 5 (max 6.7 8.9))
-- vs --
-- (sum . replicate 5) max 6.7 8.9
-- vs --
-- sum . replicate 5 $ max 6.7

-- More exmamples

-- replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))

-- replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]
