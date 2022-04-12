module Main
where
import Control.Parallel

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
fib n = (x `par` y) `pseq` (x+y)
 where
 x = fib (n-1)
 y = fib (n-2)

mkList :: Int -> [Int]
mkList n = [1..n-1]

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

euler :: Int -> Int 
euler n = length (filter(relprime n)(mkList n))

sumEuler :: Int -> Int
sumEuler = sum . (map euler) . mkList

sumFibEuler :: Int -> Int -> Int 
sumFibEuler a b = fib a + sumEuler b

parSumFibEuler :: Int -> Int -> Int
parSumFibEuler a b
 = (f `par` e) `pseq` (e+f)
 where
 f = fib a
 e = sumEuler b

r1 :: Int 
r1 = sumFibEuler 45 5300

main :: IO()
main
 = do 
 r1 `pseq` (return ())
 putStrLn("sum: "++show r1)
