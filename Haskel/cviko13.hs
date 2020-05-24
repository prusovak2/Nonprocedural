-- TODO: naprogramujte pomocí <$> a <*> (a nebo pomocí pure a <*>) funkci:
--       liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
--       která "pozvedne" funkci dvou argumentů (a -> b -> c) tak, aby fungovala
--        s argumenty v kontejnerech `f` a vracela výsledek zabalený také v `f`
-- > liftA2 (+) (Just 2) (Just 3)
-- Just 5
liftA2 ::Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 f x y = pure f <*> x <*> y

-- mohli bychom mít také aplikativní funktor Result, který bude uchovávat hodnotu a případné chyby (variace na Either)
data Result a = Error [String] | OK a
    deriving (Show)

instance Functor Result where
    fmap _ (Error e) = Error e
    fmap f (OK x) = OK (f x)

instance Applicative Result where
    pure = OK
    OK f <*> OK x = OK (f x)
    Error e <*> OK _ = Error e
    OK _ <*> Error e = Error e
    Error ef <*> Error ex = Error (ef ++ ex)

-- A co seznamy? I ty jsou aplikativní. U těch operace <*> dělá to, že vezme každou funkci, tu provede na každý prvek druhého seznamu a všechny takto vzniklé seznamy spojí dohromady. Funkce pure je jednodušší, ta prostě vrátí jednoprvkový seznam.

-- TODO: Naprogramujte <*> na seznamech. (můžete použít list comprehension)
-- > [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
-- > (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

--instance Applicative [] where
--    pure x = [x]
--    fs <*> xs = [f x | f<- fs, x <- xs]
    --vsechny fce na vsechny prvky seznamu

--instance Applicative [] where
--    pure x = [x]
 --   fs <*> xs = [f x | (f,x) <- zip fs xs]
    --prvni fce na prvni prvek seznamu atd. 
    --alternativni, stejne validni definice - zalezi na situaci

-- Monad typeclass
-- Monády = popisují výpočty, které lze skládat dohromady

-- motivace:
-- jednoducha verze, stejna jako lookup z Data.List
my_lookup :: Eq k => k -> [(k, v)] -> Maybe v
my_lookup _ [] = Nothing
my_lookup k ((l, v):r)
  | k == l = Just v
  | otherwise = my_lookup k r
-- > my_lookup 2 [(1, "ahoj"), (3, "test"), (2, "tady")]
-- Just "tady"

--4nasobny lookup
--(prvni nalezenou hodnotu pouzije jako klic pro druhe hledani, atd.)
-- *Main> my_lookup4 1 [(1,2), (3,4), (2,3), (4,999)]
-- Just 999
my_lookup4 :: Eq a => a -> [(a, a)] -> Maybe a
my_lookup4 key1 list =
  case my_lookup key1 list of
    Nothing -> Nothing
    Just key2 ->
      case my_lookup key2 list of
        Nothing -> Nothing
        Just key3 ->
          case my_lookup key3 list of
            Nothing -> Nothing
            Just key4 -> my_lookup key4 list

--pomucka
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing f = Nothing
andThen (Just a) f = f a

--trochu lepsi my_lookup4
my_lookup4' k list =
    my_lookup k list `andThen` \k ->
    my_lookup k list `andThen` \k ->
    my_lookup k list `andThen` \k ->
    my_lookup k list

cislo :: IO ()
cislo = do
    putStrLn "Yadej cislo:"
    str <- getContents 
    let vys = (read str) + 42
    putStrLn vys
 