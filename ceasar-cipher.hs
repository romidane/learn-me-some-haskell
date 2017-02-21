import           Data.Char

wrapAround :: Int -> Char -> Bool
wrapAround shift char
    | isLower char && charOrd > ord 'z' = True
    | isUpper char && charOrd > ord 'Z' = True
    | otherwise = False
    where charOrd = ord char + shift


-- is this a letter to be ciphered?
shouldCipher :: Char -> Bool
shouldCipher c =
    isLetter c && isAscii c


-- enciphers single char at a time
cipherChar :: Int -> Char -> Char
cipherChar shift char
    | shouldCipher char = chr(ord char + adjustedShift)
    | otherwise = char
    where adjustedShift = let shift' = shift `mod` 26
                          in if wrapAround shift' char
                          then shift' - 26 else shift'



-- encipher a whole string
cipher :: Int -> String -> String
cipher shift = map (cipherChar shift)


decipher :: Int -> String -> String
decipher shift = cipher (-shift)


-- Testing with QuickCheck
-- quickCheck ((\n s -> decipher n (cipher n s) == s) :: Int -> String -> Bool)
-- verboseCheck ((\n s -> decipher n (cipher n s) == s) :: Int -> String -> Bool)
