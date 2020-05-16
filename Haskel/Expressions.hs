import Data.List

data Op = Plus | Minus | Times 
  deriving (Eq, Ord)

data Exp = Const Int | Oper Op Exp Exp
  deriving (Eq, Ord)

instance Show Op where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"

instance Show Exp where
    show (Const x) = show x ++"c"
    show (Oper op arg1 arg2) = strArg1 ++" "++ show op++ " " ++ strArg2
        where
            strArg1 
               | isConst arg1 = show arg1
               | otherwise = "(" ++ show arg1 ++ ")"
            strArg2 
               | isConst arg2 = show arg2
               | otherwise = "(" ++ show arg2 ++ ")"

isConst :: Exp -> Bool
isConst (Const _ ) = True
isConst _ = False

intToConst :: Int -> Exp
intToConst n = (Const n)

listIntToListConst :: [Int] -> [Exp]
listIntToListConst = map intToConst

split :: [a] -> [([a],[a])]
split [x] = error "split: list too short"
split xs = [(splitAt len xs )| len <- [1..((length xs)-1)] ]

--tryAll :: Exp -> Exp -> (Exp,Exp,Exp)
--tryAll ex1 ex2 = (plus, minus, mult) where
--    plus = (Oper Plus ex1 ex2)
--    minus = (Oper Minus ex1 ex2)
--    mult = (Oper Times ex1 ex2)
operators = [Plus, Minus, Times]

generate:: [Exp] -> [Exp]
generate [(Const a)] = [(Const a)]
generate xs = [ (Oper op arg1 arg2) | (list1, list2) <- split xs , op <- operators, arg1 <- (generate list1), arg2 <- (generate list2)  ]

generateArit :: [Int] -> [Exp]
generateArit xs = generate $ listIntToListConst xs

evalOp :: Op -> Int -> Int -> Int
evalOp Plus a b = (a+b)
evalOp Minus a b = (a-b)
evalOp Times a b = (a*b)

eval :: Exp -> Int
eval (Const n) = n
eval (Oper op arg1 arg2) = evalOp op (eval arg1) (eval arg2) 


--generate xs = [ [(Oper op arg1 arg2) |  op <- operators, arg1 <- (generate list1), arg2 <-(generate list2) ] | (list1, list2) <- split xs ]
--generate :: [Int] -> [Exp]
--generate [x] = (Const x)
--generate xs = split xs
