dots :: Int -> Maybe String
dots x | x < 0 = Nothing
dots x = Just (replicate x '.')

x :: Maybe Int
x = Just 3

-- How to apply dots to x?

-- What result do we expect for these inputs:

-- Nothing:
-- Just (-2):
-- Just 3:

-- So we want to combine
--   (Int -> Maybe String)  and
--   Maybe Int              to get
--   ???





-- We have a function (dots) and a value in a context
-- maybe use fmap???





-- fmap (specialised) is this:
-- (Int -> Maybe String) -> Maybe Int -> Maybe (Maybe String)

-- The function we wanted:
-- (Int -> Maybe String) -> Maybe Int -> Maybe String


-- now on to io3.hs
