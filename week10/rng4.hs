import State

-- Parameters for linear congruential RNG.
a = 1664525
c = 1013904223
m = 2^32

type InternalState = Int

-- Old version of diceRolls:
--
-- diceRolls :: Int -> InternalState -> ([Int], InternalState)
-- diceRolls 0 internal = ([], internal)
-- diceRolls n internal =
--   let (x, internal2) = d6 internal
--       (xs, finalInternal) = diceRolls (n-1) internal2
--   in (x:xs, finalInternal)

d6 :: State InternalState Int
d6 = nextRandom 1 6

diceRolls :: Int -> State InternalState [Int]
diceRolls n = sequenceA (replicate n d6)



-- Very bad random number generator.
-- Generate a number between `l` and `u`.
-- Let's rewrite with new type.
nextRandom :: Int -> Int -> State InternalState Int
nextRandom l u = do
  -- Read the current state value.
  internal <- get
  let nextInternal = (a*internal + c) `mod` m
      output = nextInternal `mod` (u-l+1) + l
  -- Write the new state value.
  put internal
  pure output

-- But what are get and put?
-- get :: ???
-- put :: ???

-- (answers below)














put :: s -> State s ()
put s = State (\_ -> ((), s))

get :: State s s
get = State (\s -> (s,s))

modify :: (s -> s) -> State s ()
modify f = put . f =<< get
