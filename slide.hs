import Data.List -- (//)

-- kaliin semua list
mul [] = 1
mul (x:xs) = x * mul xs

-- duplicate s sebanyak n
dup 0 s = ""
dup n s = s ++ dup (n-1) s

-- Maybe // slide 5-5
safeDiv x 0 = Nothing
safeDiv x y = Just (x/y)

-- append
gabung x y = x : y -- gabung 1 [2,3] = [1,2,3]

-- map
map_a lis = map (+1) lis -- lis = [1,2,3] -> [2,3,4]
map_b lis = map f lis
    where f x = x+1

-- arithmatic seq (as)
as1 = [1..6]
as2 = [1,3..9]
as3 = [5,4..1]
as4 = take 5 [1,2..]

-- fold
-- foldl (/) 64 [2,4,8] -> ((64/2)/4)/8
-- foldr (/) 64 [2,4,8] -> 2/(4/(8/64))
listSum xs = foldr (+) 0 xs 

-- list compre (lc)
-- [yang ke listnya | kondisi]
lc1 xs = [x+1 | x <- xs] -- [1,2,3] -> [2,3,4]
lc2 xs ys = [x+y | x <- xs, y <- ys] -- [1,2] [2,2,2] -> [3,3,3,4,4,4]
lc3 xs = [x+2 | x <- xs, x > 3] -- [2,3,4] -> [6]
lc4 xys = [x+3 | (x,_) <- xys] -- [(1,39912313)] -> [4]
lc5 xys = [x+4 | (x,y) <- xys, x+y < 5] -- [(1,2),(3,2)] -> [5]
lc6 mxs = [x+5 | Just x <- mxs]
lc7 xys = [(x,y) | (x,y) <- xys, x+y == 5] -- [(3,2),(1,4),(6,1)] -> [(3,2),(1,4)]

-- Section 
succ1 = \x -> x+2 -- succ1 2 = x+2 = 4

-- composition (.)
compo x = (double . square) x -- = double(square(x))
    where 
        double x = x+x
        square x = x*x

add [] []         = []
add (a:as) (b:bs) = (a+b) : (add as bs)

fibs  =  1 : 1 : add fibs (tail fibs)

-- cari element terakhir di list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- cari element terakhir ke-2 di list
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

-- cek palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- reverse list menggunakan fold
reverseF :: [a] -> [a]
reverseF = foldl (flip (:)) []

-- kompres
compress (x:[]) = [x]
compress (x:xs) = if (x == head xs) then (compress xs) else x : (compress xs)
compress' x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) [head x] x

-- op \\
-- >>> [1,2,3] \\ [2]
-- [1,3]

-- notes
-- (+) 2 3 = 5
-- (++) [2] [3] = [2,3]
-- (:) 2 [3] = [2,3]
-- concat [[2],[3]] = [2,3]
-- head [1,2,3] = 1
-- tail [1,2,3] = [2,3]
-- last [1,2,3] = 3