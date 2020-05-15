--class Foldable (t :: * -> *) where
--     foldr :: (a -> b -> b) -> b -> t a -> b

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Foldable Tree where
    foldr func initValue EmptyTree = initValue
    foldr func initValue (Node x l r) = foldr func xandr_value l
        where
            xandr_value = func x r_value
            r_value = foldr func initValue r

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = (Node (f x) fl fr)
        where
            fl = fmap f l
            fr = fmap f r 

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show) 

leftToRight :: Either a b -> (a -> b) -> Either a b
leftToRight (Left x)  f = (Right (f x))
leftToRight (Right x) f = Right x

instance Functor (Either a) where
    fmap :: (a->b) -> t a -> t b
    fmap f (Left x) = x
    fmap f (Right x) = f x