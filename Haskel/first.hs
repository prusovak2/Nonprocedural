obvod a b = 2 * (a + b)
soucet2D (x1,y1) (x2,y2) = (x1+x2, y1+y2)

faktorial 1=1
faktorial x = x * faktorial (x-1)

delka [] = 0
delka (x:xs) = 1 + delka xs

--using : to construct array
zdvojnasob [] = []
zdvojnasob (x:xs) = (2*x):(zdvojnasob xs)

--urceni typu fce
my_map :: (a -> b) -> [a] -> [b]
--podtrzitko - promenna, 
my_map _ [] = []
my_map f (x:xs) = (f x):(my_map f xs)

lichy x = mod x 2 == 1

lichySeznam xs = my_map lichy xs

lichySez2 = map lichy

faktorial2 n 
    | n < 0     = error "zaporny vstup"
    | n == 1    = 1
    | otherwise    = n * faktorial(n-1)
 
lastElement :: [a] -> a  
lastElement [x] = x
lastElement (_:xs) = lastElement xs
lastElement [] = error "no elements"

--nty :: int -> [a] -> a
nty _ [] = error "not enought of elements"
nty 0 (x:xs) = x
nty n (x:xs) = nty (n-1) xs

my_zip [] x = []
my_zip x [] = []
my_zip (x:xs) (y:ys) = (x,y):(my_zip xs ys)

dedup (x:[]) = [x]
dedup (x:(y:s))
    |x == y     = dedup (y:s)
    |otherwise  = x:(dedup (y:s))
    
my_product :: [int] -> int
my_product = foldl (*) 1
