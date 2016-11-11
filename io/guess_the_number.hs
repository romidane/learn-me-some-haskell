import           Control.Monad (unless)
import           System.Random

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 100) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 100 am I thinking of? "
    numberString <- getLine
    unless (null numberString) $ do
        let number = read numberString
        if number == randNumber then putStrLn "You are correct!"
        else if number > randNumber then putStrLn "My number is smaller than that"
        else putStrLn "My number is greater than that"

        if number == randNumber then askForNumber newGen
        else askForNumber gen




-- A different way to write this function


-- main = do
--     gen <- getStdGen
--     let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
--     putStrLn "Which number in the range from 1 to 10 am I thinking of? "
--     numberString <- getLine
--     when (not $ null numberString) $ do
--         let number = read numberString
--         if randNumber == number
--             then putStrLn "You are correct!"
--             else putStrLn $ "Sorry, it was " ++ show randNumber
--         newStdGen
--         main

-- using reads to prevent errors
-- reads "12" :: [(Int, String)]
-- [(12,"")]
-- reads "a12" :: [(Int, String)]
-- []
