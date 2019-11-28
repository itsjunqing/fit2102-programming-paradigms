-- Some fun actions.
sayHi :: IO ()
sayHi = putStrLn "Hi, what's your name?"
readName :: IO String
readName = getLine
greet :: String -> IO ()
greet name = putStrLn ("Nice to meet you, " ++ name ++ ".")

-- Let's try to write this function.
meetAndGreet :: IO ()
meetAndGreet = sayHi >>= readName >>= greet
-- Doesn't typecheck.  :(
-- Repair and put into do-notation.
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--  (>>) :: Monad m => m a -> m b -> m b







-- Another example.
program =
  putStrLn "What's your name?"  >>
  getLine                       >>= \name ->
  putStrLn ("Hi, " ++ name)     >>
  putStrLn ("How old are you?") >>
  getLine                       >>= \age ->
  if read age >= 100
  then putStrLn "Wow, that's old!"
  else pure ()
  
program2 = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hi, " ++ name)
  putStrLn ("How old are you?")
  age <- getLine
  if read age >= 100
    then putStrLn "Wow, that's old!"
    else pure ()

-- Now see maybe3.hs
