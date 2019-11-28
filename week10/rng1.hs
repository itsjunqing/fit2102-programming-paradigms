-- Parameters for linear congruential RNG.
a = 1664525
c = 1013904223
m = 2^32

type InternalState = Int

-- Very bad random number generator.
-- Generate a number between `l` and `u`.
nextRandom :: Int -> Int -> InternalState -> (Int, InternalState)
nextRandom l u internal =
  let nextInternal = (a*internal + c) `mod` m
      n = nextInternal `mod` (u-l+1) + l
  in (n, nextInternal)

-- Roll a six-sided die once.
d6 :: InternalState -> (Int, InternalState)
d6 = nextRandom 1 6

-- Roll a six-sided die `n` times.
-- Have to thread state value.
-- Easy to make mistakes -- internal vs interal2, etc.
diceRolls :: Int -> InternalState -> ([Int], InternalState)
diceRolls 0 internal = ([], internal)
diceRolls n internal =
  let (x, internal2) = d6 internal
      (xs, finalInternal) = diceRolls (n-1) internal2
  in (x:xs, finalInternal)

-- back to slides!



-- nextRandom2 :: Int -> Int -> MyState InternalState Int
-- nextRandom2 l u = do
--   internal <- getState
--   let internal2 = (a*internal + c) `mod` m
--       x = internal2 `mod` (u-l+1) + l
--   putState internal2
--   pure x

-- spinRNG :: MyState InternalState ()
-- spinRNG = do
--   internal <- getState
--   putState ((a*internal + c) `mod` m)

-- spinRNG2 :: MyState InternalState ()
-- spinRNG2 = modifyState (\s -> ((a*s + c) `mod` m))

-- nextRandom3 :: Int -> Int -> MyState InternalState Int
-- nextRandom3 l u = do
--   spinRNG2
--   (\s -> s `mod` (u-l+1) + l) <$> getState

-- d6 = nextRandom3 1 6

-- diceRolls2 :: Int -> MyState InternalState [Int]
-- diceRolls2 n = do
--   sequenceA (replicate n d6)

-- data MyState s a = MyState (s -> (s, a))

-- putState :: s -> MyState s ()
-- putState s = MyState (\_ -> (s, ()))

-- getState :: MyState s s
-- getState = MyState (\s -> (s,s))

-- modifyState :: (s -> s) -> MyState s ()
-- modifyState f = putState . f =<< getState

-- runState :: MyState s a -> s -> (s, a)
-- runState (MyState f) i = f i

-- instance Functor (MyState s) where
--   fmap f (MyState sa) =
--     MyState (\s -> let (s2, a) = sa s
--                    in (s2, f a))

-- instance Applicative (MyState s) where
--   pure x = MyState (\s -> (s, x))
--   (MyState sf) <*> (MyState sx) =
--     MyState (\s -> let (s2, f) = sf s
--                        (s3, x) = sx s2
--                    in (s3, f x))

-- instance Monad (MyState s) where
--   return x = MyState (\s -> (s, x))
--   -- (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
--   (MyState sa) >>= f = MyState (\s -> let (s2, a) = sa s
--                                           MyState g = f a
--                                           (s3, b) = g s2
--                                       in (s3, b))
