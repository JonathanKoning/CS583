import Control.Parallel
import System.Random

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (losort `par` hisort) `pseq` (losort ++ x:hisort)
 where
 losort = quicksort[y | y <- xs, y<x]
 hisort = quicksort[y | y <- xs, y>=x]

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` forceList xs

main :: IO()
main
 = do
   let input = (take 20000 (randomRs (0,100)(mkStdGen 42)))::[Int]
   seq (forceList input)(return ())
--    putStrLn("sum: "++show input)
   let r = sum(quicksort input)
   seq r (return())
   putStrLn("sum: "++show r)