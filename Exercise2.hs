type Row a = [a]
type Table a = [Row a]

type Pic = Table Bool

-- 1)
mapT :: (a->b) -> Table a -> Table b
mapT f x = map (map f) x

-- 2)
mergeTables :: (a -> b -> c) -> Table a -> Table b -> Table c
mergeTables f x y = zipWith (zipWith f) x y

-- 3)
union :: Pic -> Pic -> Pic
union x y = mergeTables (||) x y