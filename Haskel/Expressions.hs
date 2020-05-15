data Op = Plus | Minus | Times 
  deriving (Eq, Ord)

data Exp = Const Int | Oper Op Exp Exp
  deriving (Eq, Ord)

instance Show Op where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"

instance Show Exp where
    show (Const x) = show x 
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