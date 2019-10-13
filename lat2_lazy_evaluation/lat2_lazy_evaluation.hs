import Data.List -- (//)

-- latihan, soal di scele

-- 1
-- >>> [x+y | x <- [1..4], y <- [2..4], x > y]
-- [5,6,7]

-- 2
divisor n = [x | x <-[1, 2 .. n], n `mod` x == 0]

-- 3, quicksort
qs [] = []
qs (x:xs) = qs [y | y <- xs, y <= x] ++ [x] ++ qs [y | y <- xs, y > x]

-- 4, permutasi
perm [] = [[]]
perm ls = [ x:ps | x <- ls, ps <- perm(ls \\ [x])]

-- 5, sieve of erastothenes
primes = sieve [2 ..]
  where sieve (x:xs) = x : (sieve [z | z <- xs, z `mod` x > 0])

-- 6, triple pythagoras
pythaTriple = [ (x,y,z) |  z <- [5 ..], y <- [z, z-1 .. 1], x <- [y, y-1 .. 1], x*x + y*y == z*z ]
