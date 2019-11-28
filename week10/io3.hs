-- Some fun actions.
sayHi :: IO()
sayHi = putStrLn "Hi, what's ur name?"

-- readName accepts a void and return IO (which is output to terminal)
readName :: IO String
readName = getLine

greet :: String -> IO ()
greet name = putStrLn ("Nice to meet you, " ++ name ++ ".")
-- What are their types?

-- Let's write this function.
meetAndGreet :: IO ()
meetAndGreet = do 
  sayHi 
  name <- readName 
  greet name
-- meetAndGreet = sayHi >> readName >>= greet

-- hint below
meetAndGreet2 :: IO ()
meetAndGreet2 = do 
  putStrLn "Hi, what's ur name?"
  name <- getLine 
  greet name









-- maybe use "conversation" approach again?
-- (from io2.hs)








-- maybe this?
meetAndGreet3 = fmap greet readName













-- Our best attempt:
-- meetAndGreet3 :: IO (IO ())

-- fmap has type:
-- (String -> IO ()) -> IO String -> IO (IO ())

-- flipped around:
-- IO String -> (String -> IO ()) -> IO (IO ())

-- We need:
-- IO String -> (String -> IO ()) -> IO ()


-- (or a function with type IO (IO ()) -> IO ())

-- back to slides!
