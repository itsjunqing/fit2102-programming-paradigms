module Tree
where

-- $setup
-- import Test.QuickCheck

-- a simple binary tree for use in binary search
data BinTree a = Nil | Node (BinTree a) a (BinTree a)

-- specialise for lookups on an Integer key, generic value
type LookupTree a = BinTree (Integer, a)

-- |
-- >>> search t 11
-- Just 'u'
-- >>> search t 8
-- Just 'i'
search :: LookupTree a -> Integer -> Maybe a
search Nil _ = Nothing
search (Node left (key, value) right) query
  | query < key = search left query
  | query > key = search right query
  | otherwise = Just value

-- simple recursive way to convert to a list
fromBinTree :: BinTree a -> [a]
fromBinTree Nil = []
fromBinTree (Node l v r) = fromBinTree l ++ [v] ++ fromBinTree r

-- | @fromBinTree@
-- >>> map snd $ fromBinTree t
-- "Yummy pineupple"

-- | @Foldable@
-- >>> foldMap (\(i,c)->[c]) t 
-- "Yummy pineupple"
-- >>> foldMap (pure.snd) t :: String
-- "Yummy pineupple"
instance Foldable BinTree where
    foldMap _ Nil = mempty
    foldMap f (Node l v r) = mconcat [foldMap f l, f v, foldMap f r]

-- | @count@
-- >>> count t
-- 15
count :: BinTree a -> Integer
count = foldr (const (1+)) 0

-- | @display@
-- >>> display t
-- Just "Yummy pineupple"
display :: LookupTree Char -> Maybe String
display t = sequenceA $ map (search t) [1 .. (count t)]

instance Functor BinTree where
    fmap _ Nil = Nil
    fmap f (Node l v r) = Node (f <$> l) (f v) (f <$> r)

instance Traversable BinTree where
    traverse _ Nil = pure Nil
    traverse f (Node l v r) = Node <$> traverse f l <*> f v <*> traverse f r

-- | @replace@
-- >>> display $ replace t 11 'a'
-- Just "Yummy pineapple"
-- >>> display $ replace t 8 'o'
-- Just "Yummy poneupple"
replace :: LookupTree a -> Integer -> a -> LookupTree a
replace Nil _ _ = Nil
replace (Node l (k,v) r) q c
  | q < k = Node (replace l q c) (k,v) r
  | q > k = Node l (k,v) (replace r q c)
  | otherwise = Node l (q,c) r

t :: BinTree (Integer,Char)
t = Node (
        Node (
            Node (Node Nil (1,'Y') Nil) (2,'u') (Node Nil (3,'m') Nil)  
        ) (4,'m') (    
            Node (Node Nil (5,'y') Nil) (6,' ') (Node Nil (7,'p') Nil)
        ) 
    ) (8,'i') (
        Node (
            Node (Node Nil (9,'n') Nil) (10,'e') (Node Nil (11,'u') Nil)
        ) (12,'p') (
            Node (Node Nil (13,'p') Nil) (14,'l') (Node Nil (15,'e') Nil)
        )
    )
