import Data.Char

-- Three "computations that can fail".

firstLetter :: String -> Maybe Char
firstLetter (c:cs) = Just c
firstLetter [] = Nothing

charAsDigit :: Char -> Maybe Int
charAsDigit c = if isDigit c
                then Just (ord c - 48)
                else Nothing

dots :: Int -> Maybe String
dots x | x < 0 = Nothing
dots x = Just (replicate x '.')

-- combined function that does "3-cat" ==> Just "..."
-- Let's write it!
combined :: String -> Maybe String
combined s = undefined

-- (answer below)
















--combined s = firstLetter s >>= charAsDigit >>= dots

-- back to slides!
