-- The laws via do-notation.

-- m >>= return  ===  m

do a <- m
   return a

-- return x >>= f === f x

do a <- return x
   f a

-- (m >>= f) >>= g  ===
--  m >>= (\x -> f x >>= g)

LHS
do a <- do x <- m
           f x
   g a

RHS
do x <- m
   a <- f x
   g a

-- Same thing with different names:
LHS
do response <- do input <- getLine
                  process input
   write response

RHS
do input <- getLine
   response <- process input
   write response
