-- Usually, when we work with algebraic expressions in school,
-- we write them in an infix manner. For instance, we write 10 - (4 + 3) * 2.
-- Addition (+), multiplication (*), and subtraction (-) are infix operators,
-- just like the infix functions in Haskell (+ `elem`, and so on).
-- As humans, we can parse this form easily in our minds.
-- The downside is that we need to use parentheses to denote precedence.

-- Another way to write algebraic expressions is to use reverse polish notation, or RPN.
-- In RPN, the operator comes after the numbers, rather than being sandwiched between them.
-- So, instead of writing 4 + 3, we write 4 3 +.
-- But how do we write expressions that contain several operators?
-- For example, how would we write an expression
-- that adds 4 and 3 and then multiplies that by 10?
-- It’s simple: 4 3 + 10 *.
-- Because 4 3 + is equivalent to 7, that whole expression is the same as 7 10 *.


-- To get a feel for how to calculate RPN expressions, think of a stack of numbers.
-- Let’s see how we would calculate the RPN expression 10 4 3 + 2 * -:

-- We push 10 onto the stack, so the stack consists of 10.

-- The next item is 4, so we push it onto the stack as well. The stack is now 10, 4.

-- We do the same with 3, and the stack is now 10, 4, 3.

-- We encounter an operator: +. We pop the two top numbers from the stack
-- (so now the stack is just 10), add those numbers together,
-- and push that result to the stack. The stack is now 10, 7.

-- We push 2 to the stack, and the stack becomes 10, 7, 2.

-- We encounter another operator. We pop 7 and 2 off the stack, multiply them,
-- and push that result to the stack.
-- Multiplying 7 and 2 produces 14, so the stack is now 10, 14.

-- Finally, there’s a -. We pop 10 and 14 from the stack, subtract 14 from 10,
-- and push that back.

-- The number on the stack is now -4. Because there are no more numbers
-- or operators in our expression, that’s our result!


-- The folding function will take a stack and an item and return a new stack.
-- We’ll use pattern matching to get the top items of a stack and to pattern match against operators like "*" and "-".
-- Here it is with the folding function implemented:

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (y*x):ys
        foldingFunction (x:y:ys) "+" = (y+x):ys
        foldingFunction (x:y:ys) "-" = (y-x):ys
        foldingFunction (x:y:ys) "/" = (y/x):ys
        foldingFunction xs numberString = read numberString:xs

