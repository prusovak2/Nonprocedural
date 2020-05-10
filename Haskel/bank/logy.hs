import Data.List

type Castka = Integer

data Operace
  = Prihlaseni
  | Odhlaseni
  | Vyber Castka
  | Pripis Castka
  deriving (Show, Read, Eq)

type Cas = Integer

type Uzivatel = String

data Zaznam =
  Zaznam Cas
         Uzivatel
         Operace
  deriving (Show, Read, Eq)

type Zaznamy = [Zaznam]

main = do
  log <- (map read . lines <$> readFile "banka.log") :: IO [Zaznam] --nacteni a rozparsovani logu
  let result cmt f --pomocna funkce na vypisovani vysledku
       = do
        putStrLn (cmt ++ ":")
        print (f log)
        putChar '\n'
  {- pocitani a vypisovani vysledku zacina zde -}
  result
    "DEMO1 -- jmeno prvniho uzivatele v souboru se smichanymi zaznamy"
    demoPrvniZaznam
  result
    "DEMO2 -- pocet zaznamu se jmenem Marie"
    demoPocetMarie
  result "mujtest" getVybery
  result "Seznam uzivatelu serazenych podle abecedy" serazeniUzivatele
  result "Casy top 10 nejvetsich vyberu" top10vyber
  result "Jmena uzivatelu 10 nejmensich pripisu" top10pripis
  result "Nejaktivnejsi uzivatel" topUzivatel
  result "Uzivatel ktery vydelal nejvic penez" topPrirustek
  result "BONUS: Prumerna vybrana castka uzivatelu zacinajicich od J" prumerVyberuJ
  result
    "BONUS: Uzivatel s nejdelsi posloupnosti akci nerusenou v logu jinymi uzivateli"
    nejdelsiSingleRun

-- příklad 1: Jméno uživatele prvního záznamu v logu
demoPrvniZaznam :: Zaznamy -> Uzivatel
demoPrvniZaznam ((Zaznam _ jm _):_) = jm

-- příklad 2: Počet záznamů se jménem Marie
demoPocetMarie :: Zaznamy -> Int
demoPocetMarie = length . filter uzivatelMarie
  where
    uzivatelMarie (Zaznam _ "Marie" _) = True
    uzivatelMarie _ = False
-- ekvivalentně:
-- demoPocetMarie zaznamy = length $ filter uzivatelMarie zaznamy
-- nebo:
-- demoPocetMarie zaznamy = length (filter uzivatelMarie zaznamy)

{- Ukol zacina tady. Misto `undefined` dodejte definice funkci, ktere z logu
 - vytahnou pozadovany vysledek. -}

getVybery x = countAverage $ sumAndLenght $ map getMoney $ filter isJUser $ filter isWithdrawal x
-- Seznam uživatelů (bez duplicit), seřazený podle abecedy
serazeniUzivatele :: Zaznamy -> [Uzivatel]
serazeniUzivatele x = nub $ map getUser (sortOn getName x ) 

-- Časy deseti největších výběrů
top10vyber :: Zaznamy -> [Cas]
top10vyber x = map getTime $ take 10 (sortOn getNegativeWithdrawal (filter isWithdrawal x) ) -- NegativeWithdrawal to sort in descending order

-- Jména uživatelů, kterým přišlo deset nejmenších přípisů (bez opakování jmen)
top10pripis :: Zaznamy -> [Uzivatel]
top10pripis x = take 5 $ nub $ map getUser (sortOn getDeposit (filter isDeposit x) ) 

-- Jméno uživatele, který je nejaktivnější (tj. má v logu nejvíc záznamů)
topUzivatel :: Zaznamy -> Uzivatel
topUzivatel x = getUserFromUsrLenTuple $ head $ sortBy (\(_, a) (_, b) -> compare b a) $ map getLenghtAndUser $ group $ map getUser (sortOn getName x )

-- Jméno uživatele, kterému na účtu přibylo nejvíc peněz (tj. má maximální součet příjmů mínus součet výdajů)
topPrirustek :: Zaznamy -> Uzivatel
topPrirustek x = getUserFromUsrTotalTuple $ maximumBy (\(_, a) (_, b) -> compare a b) ( map ( foldr (addOrSubtract) ("abraka", 0) ) ( groupBy isSameUser (sortOn getName $ filter isDepositOrWithdrawal x ) ) )

-- BONUS: Průměrná částka (oříznutá na celé číslo), kterou vybrali uživatelé začínající od J
prumerVyberuJ :: Zaznamy -> Castka
prumerVyberuJ x = countAverage $ sumAndLenght $ map getMoney $ filter isJUser $ filter isWithdrawal x

-- BONUS: Jméno uživatele, který provedl nejvíc akcí za sebou bez toho, aby jakýkoliv jiný uživatel cokoliv udělal (tj. po seřazení logu podle času bude mít “nejvíc řádků po sobě”)
nejdelsiSingleRun :: Zaznamy -> Uzivatel
nejdelsiSingleRun x = getUserFromUsrLenTuple $ maximumBy (\(_, a) (_, b) -> compare a b) ( map getLenghtAndUser $ group $ map getName $ sortOn getTime x )

getName :: Zaznam -> String
getName (Zaznam _ name _ ) = name

getUser :: Zaznam -> Uzivatel
getUser (Zaznam _ user _ ) = user

getTime :: Zaznam -> Cas
getTime (Zaznam time _ _ ) = time

getMoney :: Zaznam -> Castka
getMoney (Zaznam _ _  (Vyber money) ) = money
getMoney (Zaznam _ _  (Pripis money) ) = money
getMoney (Zaznam _ _ _ ) = error "Invalid Record"

getNegativeWithdrawal :: Zaznam -> Castka
getNegativeWithdrawal (Zaznam _ _  (Vyber money) ) = money * (-1)
getNegativeWithdrawal (Zaznam _ _ _ ) =  0

getWithdrawal :: Zaznam -> Castka
getWithdrawal (Zaznam _ _  (Vyber money) ) = money 
getWithdrawal (Zaznam _ _ _ ) =  0

isWithdrawal :: Zaznam -> Bool
isWithdrawal (Zaznam _ _  (Vyber _) ) = True
isWithdrawal (Zaznam _ _ _ ) = False

isSameUser :: Zaznam -> Zaznam -> Bool
isSameUser (Zaznam _ user1 _ ) (Zaznam _ user2 _ ) = user1 == user2


getDeposit :: Zaznam -> Castka
getDeposit (Zaznam _ _  (Pripis money) ) = money 
getDeposit (Zaznam _ _ _ ) =  0

isDeposit :: Zaznam -> Bool
isDeposit (Zaznam _ _  (Pripis _) ) = True
isDeposit (Zaznam _ _ _ ) = False

isDepositOrWithdrawal :: Zaznam -> Bool
isDepositOrWithdrawal (Zaznam _ _  (Pripis _) ) = True
isDepositOrWithdrawal (Zaznam _ _  (Vyber _) ) = True 
isDepositOrWithdrawal (Zaznam _ _ _ ) = False

isJUser :: Zaznam -> Bool
isJUser (Zaznam _ user _ ) = isJName user

isJName :: Uzivatel -> Bool
isJName ('J': _ ) = True
isJName _ = False

--takes list of multiple occurences of one user and returns User and number of its occurences
getLenghtAndUser :: [Uzivatel] -> (Uzivatel, Int)
getLenghtAndUser x = (head x, length x)

getUserFromUsrLenTuple :: (Uzivatel, Int) -> Uzivatel
getUserFromUsrLenTuple (usr, _ ) = usr

getUserFromUsrTotalTuple :: (Uzivatel, Integer) -> Uzivatel
getUserFromUsrTotalTuple (usr, _ ) = usr

addOrSubtract :: Zaznam -> (Uzivatel, Integer) -> (Uzivatel, Integer)
addOrSubtract (Zaznam _ usr  (Pripis  money) ) (_, total) = (usr, ((+) total money)) 
addOrSubtract (Zaznam _ usr  (Vyber money) ) (_, total) = (usr, ((-) total money)) 
addOrSubtract (Zaznam _ _ _ ) (_ , _) = error "Invalid Record"

sumAndLenght ::  [Integer] -> (Integer, Int)
sumAndLenght xs = (soucet, delka)
  where
    soucet = foldr (+) 0 xs
    delka = length xs 

countAverage :: (Integer, Int) -> Integer
countAverage (sm , len) = (div) sm  (toInteger len)  



