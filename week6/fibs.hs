-- Approach 1: declaring the base cases without any special syntax
fibs 0 = 1
fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2)

-- Approach 2: declaring the base cases by using an auxilary function, approach below is tail recursion implementation where the base cases are set 
fibonacci n = fibs n 1 1 
 where 
 fibs 0 a b = a
 fibs n a b = fibs(n-1) (b) (a+b)

doubleMe x = x + x

doubleUs x y = x * 2 + y * 2
doubleUsa x y = doubleMe x + doubleMe y

doublesmall x = if x > 100
 then x 
 else x*2

doublesmall2 x = (if x > 100 then x else x*2) + 1

conan = "It's me lol"

-- lucky :: (Integral a) => a -> String
-- lucky 7 = "LUCKY NUMBER SEVEN!"
-- lucky x = "Sorry, you're out of luck"

-- fromList :: [Int] -> String
-- fromList ([Int] a) = "Hello world"