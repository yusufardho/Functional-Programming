-- A: Bahasa Aritmatika Sederhana

-- 1
data Tree a = Leaf a 
    | Branch (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1) (mapTree f t2)

foldTree :: (a -> a -> a) -> (b -> a) -> Tree b -> a
foldTree combine leafFn (Leaf x) = leafFn x
foldTree combine leafFn (Branch t1 t2) = 
    combine (foldTree combine leafFn t1)
            (foldTree combine leafFn t2)

-- 2
subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1)         = if (v0 == v1) then e0 else (V v1)
subst v0 e0 (C c)          = (C c)
subst v0 e0 (e1 :+ e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :* e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :/ e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2) -- asumsi

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2)    = evaluate e1 + evaluate e2
evaluate (e1 :- e2)    = evaluate e1 - evaluate e2
evaluate (e1 :* e2)    = evaluate e1 * evaluate e2
evaluate (e1 :/ e2)    = evaluate e1 / evaluate e2
evaluate (Let v e0 e1) = evaluate (subst v e0 e1)
evaluate (V v)         = 0.0 

-- ilustrasi:
-- evaluate (Let "y" (C 9) (Let "x" (C 5) (V "x" :+ V "y" :+ C 7)))
-- evaluate (Let "x" (C 5) (V "x" :+ C 9 :+ C 7))
-- evaluate (subst "x" (C 5) (V "x" :+ V "y" :+ C 7)))
-- evaluate (C 5 :+ C 9 :+ C 7)))
-- 21

-- 3
data Expr = C Float | Expr :+ Expr | Expr :- Expr
          | Expr :* Expr | Expr :/ Expr 
          | V [Char]
          | Let String Expr Expr      
     deriving Show

-- 4
-- 5
-- 6
-- 7
-- 8
-- 9
-- 10