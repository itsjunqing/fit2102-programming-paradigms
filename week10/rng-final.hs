import Control.Monad

a = 1664525
c = 1013904223
m = 2^32

type InternalState = Int

-- Pretty type alias.
type RNG = State InternalState

-- Entire RNG
nextRandom :: Int -> Int -> RNG Int
nextRandom l u = do
  modify (\s -> ((a*s + c) `mod` m))
  (\s -> s `mod` (u-l+1) + l) <$> get

-- Example of use.
d6 :: RNG Int
d6 = nextRandom 1 6

diceRolls :: Int -> RNG [Int]
diceRolls n = do
  sequenceA (replicate n d6)

randomList :: Int -> RNG [Int]
randomList maxSize = do
  n <- nextRandom 0 maxSize
  replicateM n (nextRandom (-n*2) (n*2))

main = do
  let (lists, _) = runState (mapM randomList [1..10]) 12345
  putStrLn "Some random lists:"
  mapM_ print lists


------------------------
-- Definition of State

data State s a = State (s -> (a, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

get :: State s s
get = State (\s -> (s,s))

modify :: (s -> s) -> State s ()
modify f = put . f =<< get

runState :: State s a -> s -> (a, s)
runState (State f) i = f i

instance Functor (State s) where
  fmap f (State sa) =
    State (\s -> let (a, s2) = sa s
                 in (f a, s2))

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  (State sf) <*> (State sx) =
    State (\s -> let (f, s2) = sf s
                     (x, s3) = sx s2
                 in (f x, s3))

instance Monad (State s) where
  return = pure
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  State sa >>= f = State (\s -> let (a, s2) = sa s
                                    State g = f a
                                    (b, s3) = g s2
                                in (b, s3))
