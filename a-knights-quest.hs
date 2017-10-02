import           Control.Monad

type KnightPos = (Int, Int)

-- Here is a function that takes the knight’s position and returns all of his next moves:
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1)
    , (c + 2, r + 1)
    , (c - 2, r - 1)
    , (c - 2, r + 1)
    , (c + 1, r - 2)
    , (c + 1, r + 2)
    , (c - 1, r - 2)
    , (c - 1, r + 2)
    ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

-- The knight can always take one step horizontally or vertically and two steps horizontally or vertically,
-- but his movement must be both horizontal and vertical.
-- (c', r') takes on every value from the list of movements
-- and then guard makes sure that the new move, (c', r'), is still on the board.
-- If it’s not, it produces an empty list, which causes a failure and return (c', r') isn’t carried out for that position.
-- This function can also be written without the use of lists as monads. Here is how to write it using filter:
-- moveKnight :: KnightPos -> [KnightPos]
-- moveKnight (c, r) = filter onBoard
--     [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
--     ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
--     ]
--     where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]
-- ghci> moveKnight (6, 2)
-- [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
-- ghci> moveKnight (8, 1)
-- [(6,2),(7,3)]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

-- If you pass it (6, 2), the resulting list is quite big.
-- This is because if there are several ways to reach some position in three moves,
-- the move crops up in the list several times.
-- Here’s the preceding code without do notation:

in3' start = return moveKnight start >>= moveKnight >>= moveKnight

-- Using >>= once gives us all possible moves from the start.
-- When we use >>= the second time, for every possible first move,
-- every possible next move is computed, and the same goes for the last move.


-- Now, let’s make a function that takes two positions and tells us if you can get from one to the other in exactly three steps:

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- We generate all the possible positions in three steps, and then we see if the position we’re looking for is among them.
-- Here’s how to check if we can get from (6, 2) to (6, 1) in three moves:

-- ghci> (6, 2) `canReachIn3` (6, 1)
-- True
-- Yes! How about from (6, 2) to (7, 3)?

-- ghci> (6, 2) `canReachIn3` (7, 3)
-- False

