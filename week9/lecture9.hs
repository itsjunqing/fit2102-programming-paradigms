
foldleft ::  (b->a->b) -> b -> [a] -> b
foldleft _ a [] = a
foldleft f a (x:xs) = foldleft (f) (f a x) (xs)

foldright :: (a->b->b) -> b -> [a] -> b
foldright _ b [] = b
foldright f b (x:xs) = f x (foldright f b xs)

-- TEST CASES 
-- foldright (+) 0 [1..10] --> 55
-- foldleft (+) 0 [1..10] == foldright (+) 0 [1..10] --> True
-- foldleft (-) 0 [1..10] --> -55
-- foldright (-) 0 [1..10] --> -5