canSumTo :: Int -> [Int] -> [Int]
canSumTo y [] = []
canSumTo y (x:xs) = [x | z<-xs, x+z==y] ++ [z | z<-xs, x+z==y] ++ canSumTo y xs

diff :: [Int] -> [Int]
diff [] = []
diff [a] = []
diff (x:y:xs) = [x-y] ++ diff (y:xs) 