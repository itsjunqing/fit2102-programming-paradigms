-- getLine :: IO String

-- What are their types?
sayHi = putStrLn "Hi, what's your name?"
readName = getLine
beNice = putStrLn "Nice to meet you."

-- chat = sequenceA [sayHi, readName, beNice]

-- Let's write this function.
-- It should say hi, then read the name, then be nice.
-- The String value should be the name entered.
conversation :: IO String
conversation = undefined

-- (hint below)







-- <*> :: f (a -> b) -> f a -> f b
-- <*> :: IO (a -> b) -> IO a -> IO b

-- _ <$> _ <*> _ <*> _ <*> _ ...


-- conversation = ??? <$> sayHi <*> readName <*> beNice










-- f :: () -> String -> () -> String
-- f () s () = s

-- back to slides!
