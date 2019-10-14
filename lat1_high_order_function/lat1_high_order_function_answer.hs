import Data.Maybe -- isJust

-- 1.1 Define the length function using map and sum.
len lis = sum(map (+1) (map (*0) lis))
-- 1.1 alternative answer
length' xs = sum $ map ($1) xs

-- 1.2
-- map (+1) (map (+1) xs) nambahin semua elemen xs+2
-- map f (map g xs) semua elemen xs = f(g(xs))

-- 1.3
iter :: Int -> (a -> a) -> (a -> a)
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- 1.4

-- 1.5 
-- How would you define the sum of the squares of the natural numbers 
-- 1 to n using map and foldr?
sumSquares :: Integer -> Integer
sumSquares n = foldr (+) 0 (map (\x -> x*x) [1..n])

-- 1.6

-- 1.7
-- If id is the polymorphic identity function, defined by id x = x, 
-- explain the behavior of the expressions

-- 1.8 Define a function composeList
composeList :: [a -> a] -> (a -> a)
composeList []     = id
composeList (f:fs) = f . composeList fs

-- 1.9 Define a function flip
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x

-- 2.1a
soal1a xs = map (+1) xs

-- 2.2a 
soal2a [] ys = []
soal2a (x:xs) ys = (map (+x) ys) ++ (soal2a xs ys)
soal2a' xs ys = concat (map (\x -> map (\y -> x+y) ys) xs)

-- 2.3a
soal3a xs = map (+2) (filter (>3) xs)                         

-- 2.4a
soal4a xys = map (\(x,_) -> x+3) xys                           
soal4a' xys = map ((+3) . fst) xys

-- 2.5a
soal5a xys = map ((+4) . fst) (filter (\(x,y) -> x+y < 5) xys)
soal5a' xys = map (\(x,_) -> x+4) (filter (\(x,y) -> x+y < 5) xys)

-- 2.6a
soal6a mxs = map (\(Just x) -> x+5) (filter isJust mxs)

-- 2.1b
soal1b xs = [ x+3 | x <- xs ]

-- 2.2b
soal2b xs = [ x | x <- xs, x > 7 ]

-- 2.3b
soal3b xs ys = [ (x,y) | x <- xs, y <- ys ]

-- 2.4b
soal4b xys = [ x+y | (x,y) <- xys, x+y > 3 ]

-- 3a

-- 3b

-- 3c

-- 4

