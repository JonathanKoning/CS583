data BinTree a = Branch a (BinTree a) (BinTree a) | Empty

-- T1 = Function
-- T2 = value
-- foldT :: T1 -> T2 -> BinTree a -> T3
foldT :: (a -> a -> a) -> a -> BinTree a -> a
foldT _ x Empty = x
foldT f x (Branch a lt rt) = foldT f (foldT f (f a x) lt) rt
-- foldT f x (Branch a lt rt) = f (f (foldT f x lt) (foldT f x rt)) a  

-- which compute the sum of a tree of numbers as an instance of foldT.
sumT :: BinTree Int -> Int
sumT = foldT (+) 0

-- which compute the maximum of a tree of numbers as an instance of foldT.
maxT :: BinTree Int -> Int
maxT = foldT (max) 0


-- levels :: BinTree a -> [[a]]
-- levels Empty = [[]]
-- levels 