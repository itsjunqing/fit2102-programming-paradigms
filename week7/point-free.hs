data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq Day where
  Sat == Sat = True
  Sun == Sun = True
  _ == Sat = False
  _ == Sun = False
  Sat == _ = False 
  Sun == _ = False
  _ == _ = True

phonebook :: [(String, String)]
phonebook = [ ("Bob",   "01788 665242"), ("Fred",  "01624 556442"), ("Alice", "01889 985333") ]


printNumber name = msg $ lookup name phonebook
  where 
    msg (Just number) = print number
    msg Nothing = print $ name ++ " not found in phonebook"


data Suit = Spade | Club | Diamond | Heart
  deriving (Eq, Enum, Ord, Show)

-- declare the type comparison 
-- instance Show Suit where
--   show Spade = "^"
--   show Club = "&"
--   show Diamond = "O"
--   show Heart = "V"

func :: Num a => a -> a -> a -> a 
func = ((+).).(*)
-- func a b c = (a*b)+c
-- func a b c = (+) (a*b) c   # operator sectioning - aka set prefix
-- func a b = (+) (a*b)       # eta reduction
-- func a b = (+) (((*) (a)) (b))    # operator sectioning
-- func a b = (+).((*) (a)) b        # similar to f.g x
-- func a = (+).((*) (a))            # eta reduction
-- func a = ((+).).(*)               # eta reduction


aFunc :: Num a => a -> a -> a -> a
aFunc = ((*).).(+)

-- aFunc a b c = (a+b) * c
-- aFunc a b c = (*) (a+b) c     # change to prefix function
-- aFunc a b = (*) (a+b)         # eta reduction, here we will receive one more parameter to be passed into multiplier
-- aFunc a b = (*) (((+) a) b)   # change to prefix function, here ((+) a) is the g function and b is the x 
-- aFunc a b = (*).((+) a) b     # using composition law
-- aFunc a = (*).((+) a)           # perform eta reduction, here, (+) is g function and a is the x
-- aFunc a = ((*).).(+) a 
-- aFunc = ((*).).(+)            # here is takes two arguments to perform the RHS of (+) operator. 