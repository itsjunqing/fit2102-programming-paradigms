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

-- With explicit bind:
previousCombined s =
  firstLetter s >>= charAsDigit >>= dots

-- This time with do-notation.
combined :: String -> Maybe String
combined s = do
  c <- firstLetter s
  d <- charAsDigit c
  dots d

-- back to slides!
