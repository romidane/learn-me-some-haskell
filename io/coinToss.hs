import           System.Random

-- Let’s make a function that simulates tossing a coin three times.
-- If random didn’t return a new generator along with a random value,
-- we would need to make this function take three random generators as a parameter
-- and return coin tosses for each of them. But if one generator can make a random value of type Int
-- (which can take on a load of different values), it should be able to make three coin tosses
-- (which can have only eight different end results).
-- So this is where random returning a new generator along with a value comes in handy.


threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thrirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thrirdCoin)

-- ghci> threeCoins (mkStdGen 21)
-- (True,True,True)
-- ghci> threeCoins (mkStdGen 22)
-- (True,False,True)
-- ghci> threeCoins (mkStdGen 943)
-- (True,False,True)
-- ghci> threeCoins (mkStdGen 944)
-- (True,True,True)

-- Notice that we didn’t need to call random gen :: (Bool, StdGen).
-- Since we already specified that we want Booleans in the type declaration of the function,
-- Haskell can infer that we want a Boolean value in this case.

-- .e.g
-- threeCoins :: StdGen -> (Double, Double, Double)

