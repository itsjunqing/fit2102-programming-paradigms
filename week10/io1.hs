-- putStrLn :: String -> IO ()

-- A value of type "IO ()" does some I/O but doesn't produce an
-- interesting value.
action1 :: IO ()
action1 = putStrLn "hello"

action2 :: IO ()
action2 = putStrLn "world"

actions :: [IO ()]
actions = [action1, action2]
-- Refresher question:
-- What function has this type:  [IO ()] -> IO [()]
-- (remember that IO is an instance of Applicative)

-- (answer below)









-- week 8 tute:
-- sequence :: Applicative f => [f a] -> f [a]

-- (a more general version is called "sequenceA"
--   in Data.Traversable)

-- specialised type:
-- sequenceA :: Applicative f => [f a] -> f [a]
-- sequenceA :: [IO a] -> IO [a]
combinedAction = sequenceA actions


hellos = sequenceA (replicate 3 action1)

-- now go to io2.hs for more fun


