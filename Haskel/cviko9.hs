devet=[90,81..0]

mensiNez n xs = [x| x<-xs, x < n]

removeNonUppercase str =[x | x <- str, elem x ['A'..'Z'] ]

dvojice n = [(x,y) | x<-[1..n], y<-[1..n] ]

pyth n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*x+y*y==z*z]

licha = [x | x <- [1..], x `mod` 2 == 1 ]

lichy = 1 : (map (+2) lichy)

fib1 a b = a:(fib1 b (a+b))
fib = 1:1:(zipWith(+) fib (tail fib))

--Vlastni typy
data Shape = Circle Double Double Double | Rectangle Double Double Double Double deriving (Show) 
area :: Shape -> Double
area (Circle _ _ r) = pi *r ^ 2

toArea :: [Shape] -> [Double]
toArea xs = map (area) xs

isCircle :: Shape -> Bool
isCircle (Circle _ _ _) = True
isCircle _ = False

--typovy konstruktor - bere jako arg typ a vytvari novy typ Complex
--specializovany na dany typ a
data Complex a = Complex a a deriving(Show)

--getter realne a imaginarni casti
real (Complex x _) = x
imag (Complex _ y) = y

data Complex2 a = C {real2:: a,imag2::a } deriving(Show)

--multComplex :: Num a => Complex a -> Complex a -> Complex a     
multComplex (C r1 i1) (C r2 i2) = C (r1*r2 - i1*i2) (r1*i2 + r2*i1) 

type Mystring = [Char]

--rekurzivni datove struktury
-- | dva ruzne konstruktory
data List a= Nil | Cons a (List a) deriving(Show)
prsez = Cons 1(Cons 2(Cons 3 Nil)) 

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving(Show)

insertS :: Ord a => Tree a -> a -> Tree a
insertS Leaf x = Node Leaf x Leaf
insertS (Node l v r) x
    | x < v     = Node (insertS l x) v r
    | otherwise = Node l v (insertS r x)

lookUp :: Tree a -> a -> Bool
lookUp a Leaf x
    |   a == x = True
    | otherwise  = False
lookUp a Node(Tree y) x (Tree z)
    | a == x = True
    | otherwise lookUp a (Tree y) || lookUp a (Tree z)