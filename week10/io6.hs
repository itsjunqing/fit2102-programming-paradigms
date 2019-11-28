import Data.Time

-- Terrible way to compute nth
-- Fibonacci number.
slowFib n | n <= 1 = n
slowFib n = slowFib (n-1) + slowFib (n-2)

-- Run an action and print how
-- long it took.
timeAction :: IO a -> IO a
timeAction action = do
  before <- getCurrentTime
  result <- action
  after <- getCurrentTime
  let elapsed = after `diffUTCTime` before
  putStrLn $ "It took " ++ show elapsed ++ " seconds."
  pure result

prog n = do
  timeAction (print (slowFib n))

prog2 n = do
  x <- timeAction (pure (slowFib n))
  print x
