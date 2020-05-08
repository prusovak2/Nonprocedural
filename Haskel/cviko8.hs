my_takeWhile :: (a->Bool)->[a]->[a]
my_takeWhile f [] = []
my_takeWhile f (x:xs) 
    | f x = x:(my_takeWhile f xs)
    | otherwise = []
    
my_filter f [] =[]
my_filter f (x:xs)
    | f x = x:(my_filter f xs)
    | otherwise = my_filter f xs
    
my_dropWhile :: (a->Bool)->[a]->[a]
my_dropWhile f [] = []
my_dropWhile f (x:xs) 
    | f x = []
    | otherwise = x:(my_dropWhile f xs)
    
my_span f xs = (takeWhile f xs, dropWhile f xs)
    
my_group:: Eq a => [a]-> [[a]]
my_group [] = []
my_group (x:xs) = (x:front):(my_group back)
    where (front, back) = my_span (==x) xs
    
my_foldr f z [] = z
my_foldr f z (x:xs) = f x (my_foldr f z xs)

my_sum xs = my_foldr (+) 0 xs

scalarMult x y = my_sum(zipWith (*) x y)

diffs :: Num a => [a] -> [a]
diffs xs = zipWith (-) (tail xs) xs

prvocisla = 2:(filter jePrvo [3..] )
    where 
        jePrvo x = and (map (nedelitelne x) (mensiPrvocisla x))
        nedelitelne x prvo = ( x `mod` prvo) /= 0
        mensiPrvocisla x = takeWhile (\p -> p*p <= x) prvocisla