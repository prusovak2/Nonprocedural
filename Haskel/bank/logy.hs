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

-- Seznam uživatelů (bez duplicit), seřazený podle abecedy
serazeniUzivatele :: Zaznamy -> [Uzivatel]
serazeniUzivatele = undefined

-- Časy deseti největších výběrů
top10vyber :: Zaznamy -> [Cas]
top10vyber = undefined

-- Jména uživatelů, kterým přišlo deset nejmenších přípisů (bez opakování jmen)
top10pripis :: Zaznamy -> [Uzivatel]
top10pripis = undefined

-- Jméno uživatele, který je nejaktivnější (tj. má v logu nejvíc záznamů)
topUzivatel :: Zaznamy -> Uzivatel
topUzivatel = undefined

-- Jméno uživatele, kterému na účtu přibylo nejvíc peněz (tj. má maximální součet příjmů mínus součet výdajů)
topPrirustek :: Zaznamy -> Uzivatel
topPrirustek = undefined

-- BONUS: Průměrná částka (oříznutá na celé číslo), kterou vybrali uživatelé začínající od J
prumerVyberuJ :: Zaznamy -> Castka
prumerVyberuJ = undefined

-- BONUS: Jméno uživatele, který provedl nejvíc akcí za sebou bez toho, aby jakýkoliv jiný uživatel cokoliv udělal (tj. po seřazení logu podle času bude mít “nejvíc řádků po sobě”)
nejdelsiSingleRun :: Zaznamy -> Uzivatel
nejdelsiSingleRun = undefined

getName :: Zaznam -> String
getName (Zaznam _ name _ ) = name