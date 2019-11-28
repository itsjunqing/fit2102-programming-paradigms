
-- sumNum [] = 0
-- sumNum (x: xs) = 0 + if mod x 6 == 0 || mod x 3 == 0 then x + sumNum xs else sumNum xs
-- sumNum (x: xs) = 0 if mod x 6 == 0 || mod x 3 == 0 then x + sumNum xs else sumNum xs
-- sumNum (x: xs) = 
--  if mod x 6 == 0 || mod x 3 == 0 then x + sumNum xs
--  else 0 + sumNum xs

-- sumNum 0 sum = sum
-- sumNum n sum = 0 + if mod n 6 == 0 || mod n 3 == 0 then n + sumNum (n-1) (sum) else sumNum

-- sumNum 1000 = 
--  let sumNum 0 x y = x

-- sumNum :: Int -> Int
--  sumNum 1000 = sumNumAux
--  	sumNum 0 a b = a
--  	sumNum n a b = sumNum (n-1) if b mod 3 == 0 || b mod 5 == 0 then b (a+b) else 0 

euler n = eulerAux n 0
 where
 eulerAux 0 sum = sum
 eulerAux n sum = eulerAux (n-1) (sum + if mod n 3 == 0 || mod n 5 == 0 then n else 0)

-- f (x-1) (y + (if x 'mod' 3 == 0 || x 'mod' 5 == 0 then x else 0))
-- sumList [] = 0
-- sumList (x : xs) = x + sumList xs