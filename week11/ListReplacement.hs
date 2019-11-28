module ListReplacement
where

-- | @replaceFirst@
-- >>> replaceFirst [1,2,3] 4
-- [4,2,3]
replaceFirst :: [a] -> a -> [a]
replaceFirst (x:xs) a = a:xs

-- | @replaceLast@
-- >>> replaceLast [1,2,3,4] 5
-- [1,2,3,5]
replaceLast :: [a] -> a -> [a]
replaceLast list a = (init list) ++ [a]
replaceLast all@(x:xs) a = (take (length all -1) all) ++ [a]