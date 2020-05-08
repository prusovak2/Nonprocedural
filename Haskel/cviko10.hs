import Data.List

--zip vytvari dvojice, prvni map vytvari delky, druhy map vezme vzdy 1 pismeno
rle :: Eq a => [a] -> [(Int, a)]
rle xs = zip (map length grouped) (map head grouped)
    where grouped = group xs
 
rle2 :: Eq a => [a] -> [(Int, a)]    
rle2 xs = map(\sled -> (length sled, head sled)) (group xs)

-- priklad na skladani fci
rle3 xs = (add_len . group . (filter odd) . nadruhou)
    where nadruhou = map (^2)
          add_len =  map(\sled -> (length sled, head sled))
          
data IntX = NegInf | PosInf | Num Int deriving (Show)

less :: IntX -> IntX -> Bool
less NegInf NegInf = False
less PosInf PosInf = False
less NegInf _ = True
less PosInf _ = False
less (Num a) (Num b) = a < b 
 
greater :: IntX -> IntX -> Bool    
greater a b = (not) ( less a b )

intxs = [Num 3, Num 10, NegInf, PosInf, Num 392, NegInf]
-- TODO: ze seznamu intxs odfiltrujte všechna nekoneèna a pøeveïte je na normální Int:
ints  = [a |(Num a) <- intxs ]

ints2 = map prevedInt $ filter isNumber intxs
    where
        isNumber (Num _) = True
        isNumber _ = False
        prevedInt (Num x) = x
        
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show, Read, Eq)

testTree = Node 10 (Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)) (Node 20 EmptyTree EmptyTree)


treeDepth :: Tree a -> Int
treeDepth EmptyTree = 0
treeDepth (Node value leftSubtree rightSubtree) = max (treeDepth leftSubtree)  (treeDepth rightSubtree) + 1