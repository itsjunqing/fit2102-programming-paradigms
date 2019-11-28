data State s a = State (s -> (a, s))

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State sa) = undefined

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure a = undefined
  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  (State sf) <*> (State sx) = undefined

instance Monad (State s) where
  return = undefined
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  State sa >>= f = undefined

-- (answers below)


























-- instance Functor (State s) where
--   -- fmap :: (a -> b) -> State s a -> State s b
--   fmap f (State sa) =
--     State (\s -> let (a, s2) = sa s
--                  in (f a, s2))

-- instance Applicative (State s) where
--   -- pure :: a -> State s a
--   pure a = State (\s -> (a, s))
--   -- (<*>) :: State s (a -> b) -> State s a -> State s b
--   (State sf) <*> (State sx) =
--     State (\s -> let (f, s2) = sf s
--                      (x, s3) = sx s2
--                  in (f x, s3))

-- instance Monad (State s) where
--   return = pure
--   -- (>>=) :: State s a -> (a -> State s b) -> State s b
--   State sa >>= f = State (\s -> let (a, s2) = sa s
--                                     State g = f a
--                                 in g s2)
