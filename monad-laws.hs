------------------------------------------------
-- Monad Laws
------------------------------------------------


-- Just like functors and applicative functors,
-- monads come with a few laws that all monad instances must abide by.
-- Just because something is made an instance of the Monad type class doesn’t mean that it’s actually a monad.
-- For a type to truly be a monad, the monad laws must hold for that type.
-- These laws allow us to make reasonable assumptions about the type and its behavior.


-- Haskell allows any type to be an instance of any type class as long as the types check out.
-- It can’t check if the monad laws hold for a type though,
-- so if we’re making a new instance of the Monad type class,
-- we need to be reasonably sure that all is well with the monad laws for that type.
-- We can rely on the types that come with the standard library to satisfy the laws,
-- but when we go about making our own monads, we need to manually check whether the laws hold.


----------------------------------------
-- Left Identity
----------------------------------------

-- The first monad law states that if we take a value,
-- put it in a default context with return, and then feed it to a function by using >>=,
-- that’s the same as just taking the value and applying the function to it.
-- To put it formally, return x >>= f is the same damn thing as f x.

-- ghci> return 3 >>= (\x -> Just (x+100000))
-- Just 100003
-- ghci> (\x -> Just (x+100000)) 3
-- Just 100003

-- ghci> return "WoM" >>= (\x -> [x,x,x])
-- ["WoM","WoM","WoM"]
-- ghci> (\x -> [x,x,x]) "WoM"
-- ["WoM","WoM","WoM"]


----------------------------------------
-- Right Identity
----------------------------------------
-- The second law states that if we have a monadic value and we use >>= to feed it to return,
-- the result is our original monadic value. Formally, m >>= return is no different than just m.

-- return puts a value in a minimal context that still presents that value as its result.
-- This means that, for instance, for Maybe, it doesn’t introduce any failure; for lists,
-- it doesn’t introduce any extra nondeterminism.

-- Here’s a test run for a few monads:

-- ghci> Just "move on up" >>= (\x -> return x)
-- Just "move on up"
-- ghci> [1,2,3,4] >>= (\x -> return x)
-- [1,2,3,4]
-- ghci> putStrLn "Wah!" >>= (\x -> return x)
-- Wah!


-- In this list example, the implementation for >>= is as follows:

-- xs >>= f = concat (map f xs)
-- So when we feed [1,2,3,4] to return, first return gets mapped over [1,2,3, 4],
-- resulting in [[1],[2],[3],[4]]. Then this is concatenated, and we have our original list.

-- Left identity and right identity are basically laws that describe how return should behave.
-- It’s an important function for making normal values into monadic ones,
-- and it wouldn’t be good if the monadic value that it produced had any more than the minimal context needed.



----------------------------------------
-- Associativity
----------------------------------------

-- The final monad law says that when we have a chain of monadic function applications with >>=,
-- it shouldn’t matter how they’re nested.
-- Formally written, doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g).
