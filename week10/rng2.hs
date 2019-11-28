-- Our new type!
data State s a = State (s -> (a, s))

-- Parameters for linear congruential RNG.
a = 1664525
c = 1013904223
m = 2^32

type InternalState = Int

-- Very bad random number generator.
-- Generate a number between `l` and `u`.
-- Let's rewrite with new type.
nextRandom :: Int -> Int -> State InternalState Int
nextRandom l u = undefined

-- (answer below)



















-- nextRandom l u = State f
--   where
--     f internal = (output, nextInternal)
--       where
--         nextInternal = (a*internal + c) `mod` m
--         output = nextInternal `mod` (u-l+1) + l

-- nextRandom2 :: Int -> Int -> State InternalState Int
-- nextRandom2 l u = State $ \internal ->
--   let nextInternal = (a*internal + c) `mod` m
--       output = nextInternal `mod` (u-l+1) + l
--   in (output, nextInternal)

-- back to the slides!
