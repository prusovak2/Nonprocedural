data TrafficLight = Red | Yellow | Green 

instance Show TrafficLight where
    --show :: TrafficLight -> String
    show Red = "Red"
    show Yellow = "Yellow"
    show Green = "Green"
    
data Tree a = EmptyTree | Node a (Tree a) (Tree a)

instance (Eq a) => Eq (Tree a) where
    (==)  EmptyTree EmptyTree = True
    (==) (Node v1 l p) (Node v2 l2 p2) = v1 == v2 && l == l2 && p == p2
    (==) _ _ = False
    

-- typová tøída emulující truthy/falsy hodnoty
class YesNo a where
    yesno :: a -> Bool

-- instance typu Int typové tøídy YesNo
--instance YesNo Int where
--    yesno 0 = False  
--   yesno _ = True
    
--instance YesNo Bool where
--    yesno = id
    
--instance YesNo Tree where
--    yesno EmptyTree = False
--    yesno _ = True
    
--instance YesNo [a] where
--    yesno [] = False
--    yesno _ = True
    
data Mozna a = Proste a | Nic -- pøeklad typu Maybe, se kterým se ještì setkáme

--class MyFunctor f where  
--    myfmap :: (a -> b) -> f a -> f b
    
instance Functor Mozna where
    fmap f Nic = Nic
    fmap f (Proste x) = Proste (f x)
    
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node v l p) = (Node (f v) (fmap f l) (fmap f p))