module Main
where
import Control.Parallel
import GHC (XBindStmtRn(xbsrn_bindOp))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
fib n = (x `par` y) `pseq` (x+y)
 where
 x = fib (n-1)
 y = fib (n-2)

mapFib :: [Int]
mapFib = map fib [37,38,39,40]

mkList :: Int -> [Int]
mkList n = [1..n-1]

relprime :: Int -> Int -> Bool 
relprime x y = gcd x y == 1

euler :: Int -> Int 
euler n = length (filter(relprime n)(mkList n))

sumEuler :: Int -> Int
sumEuler = sum . (map euler) . mkList

mapEuler :: [Int]
mapEuler = map sumEuler [7600,7600]

parMapFibEuler :: Int
parMapFibEuler = (forceList mapFib) `par`
                 (forceList mapEuler `pseq` (sum mapFib + sum mapEuler))

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` forceList xs

main :: IO()
main
 = putStrLn (show parMapFibEuler)