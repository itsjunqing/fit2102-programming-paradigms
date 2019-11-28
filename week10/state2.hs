data State s a = State (s -> (a, s))


-- Let's write this.
runState :: State s a -> s -> (a, s)
runState = undefined




instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State sa) =
    State (\s -> let (a, s2) = sa s
                 in (f a, s2))

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure a = State (\s -> (a, s))
  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  (State sf) <*> (State sx) =
    State (\s -> let (f, s2) = sf s
                     (x, s3) = sx s2
                 in (f x, s3))

instance Monad (State s) where
  return = pure
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  State sa >>= f = State (\s -> let (a, s2) = sa s
                                    State g = f a
                                in g s2)
