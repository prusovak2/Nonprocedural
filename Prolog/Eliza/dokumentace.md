#Eliza 
Cílem zápočtového programu bylo v programovacím jazyce `Prolog` naimplementovat program `Eliza`, který se prostřednictvím pattern matchingu a substituce pokouší simulovat co nejpřirozenější konverzaci, aniž by znal význam uživatelem psaných slov.  Součástí zadání bylo, aby `Eliza` disponovala širokou paletou vzorů odpovědí, mezi kterými umí plynule střídat a aby implementovala jednoduchou paměť, do které si ukládá některé vstupy uživatele obsahující významná slova. Obsah této paměti následně umí použít v odpovědi.

První `Eliza` byla napsána v letech 1964 -1966 profesorem Josephem Weizenbaumem a byla pojmenována dle postavy Eliza Doolittle z komedie George Bernanda Shawna Pygmalion.
																																*[Wikipedie]*

##Implementace

Predikáty programu lze z dle účelu pomyslně rozdělit do tří částí. První podmnožina z nich se stará o parsování vstupu, druhá implementuje logiku, odpovědnou za formulaci odpovědi pro uživatele a třetí odpověď vypíše na výstup. Nebudu zde do detailu rozebírat účel a fungování každého jednotlivého predikátu, neboť tento účel už plní komentáře ve zdrojovém kódu. Shrnu raději princip fungování jednotlivých částí programu a pozastavím se nad zajímavými implementačními detaily.

### Parser uživatelského vstupu

Parser je z velké části převzatý z implementace z přednášky. Vstup je čten po znacích, predikát `readLetters` čte písmena dokud nedetekuje konec slova (zde jsem implementaci rozšířila o větší škálu `charů`, jež slovo ukončí). Predikát `readRest`čte slova věty, dokud nenarazí na tečku, otazník či vykřičník, což jsou jediné `chary` indikující konec věty.

Za zmínku stojí preprocessing, který je v rámci parsování s uživatelskou větou proveden. Všechna písmena všech slov jsou převedena do lower case. To zaprvé umožňuje Elize namatchovat slova ze vstupu bez ohledu na to, zda je uživatel zadal v lower či upper case, aniž by bylo třeba jakkoli komplikovat logiku programu. Zadruhé se tak zbavím velkých písmen na začátcích slov. To je podstatné, neboť slova jsou v programu reprezentována `atomy` a velké písmeno na začátku by způsobilo zaměnění za Prologovskou proměnnou. Z podobného důvodu jsou všechna čísla v uživatelském vstupu převedena na `string`. Logika budující odpověď používá čísla ve vzorech vět a odpovědí a provádí jejich prostřednictvím substituce slov z  uživatelem zadané věty do věty na výstupu. Proto jsme shledala za rozumnější se čísel ve vstupu zbavit, abych se vyvarovala jejich dezinterpretace za interní symboly a následného zmatení algoritmu budujícího odpověď.

### Budování odpovědi

**Proces budování odpovědi lze rozdělit do následujících fází:**

1. zjednodušení vstupní věty
2. nalezení klíčových či důležitých slov
3. nalezení vzoru odpovídajícího vstupní větě a nalezenému klíčovému slovu 
4. výběr vzoru odpovědi pro dané klíčové slovo a vzor vstupní věty

#### Zjednodušení vstupní věty
K zjednodušení vstupní věty slouží predikát `simplify` spolu se sadou pravidel  reprezentovaných predikátem `simplificationRule`.

Každé zjednodušovací pravidlo slouží jednomu z následujících účelů. Některá převádí slovo s významem podobným některému klíčovému slovu na příslušné klíčové slovo, což `Elize` umožňuje validně reagovat na větší škálu slov s menší naimplementovanou množinou klíčových slov.
```Prolog
simplificationRule([alike|X],[similar|Y],X,Y).
simplificationRule([same|X],[similar|Y],X,Y).
simplificationRule([resemble|X],[similar|Y],X,Y).
simplificationRule([remind,me,of|X],[similar|Y],X,Y).
```
Další dovolují `Elize` převádět mezi zájmeny, aby byla schopna produkovat přirozenější odpovědi. Uvede-li uživatel ve vstupní větě například zájmeno `you`, evidentně referuje k `Elize` samotné a ona tedy v odpovědi užije zájmeno `I`. 

Zjednodušení je nutno vzít v potaz při vytváření vzorů odpovědí příslušných klíčovým slovům. Klíčová slova jsou totiž hledána až ve zjednodušené verzi vstupní věty. Klíčové slovo `you` tedy odpovídá situaci, kdy uživatel uvedl na vstupu `I` a tomu je třeba přizpůsobit příslušné vzory odpovědí.

```Prolog
simplificationRule([me|X],[you|Y],X,Y).
simplificationRule([you,'\'',re|X],[im|Y],X,Y).
simplificationRule([myself|X],[yourself|Y],X,Y).
simplificationRule([yourself|X],[myself|Y],X,Y).
```
####Nalezení klíčových slov
Klíčovými slovy míním slova, pro která `Eliza` disponuje nějakou sadou vzorů odpovědí. Pokud žádné takové není ve větě nalezeno, je použito speciální  `none`. Důležitá slova  (označená predikátem `important`) naproti tomu souvisí s implementací jednoduché paměti a bude o nich zmínka později. 

Každé klíčové slovo je označeno predikátem `keyword(keyword, priority)`.  Za nalezení klíčových slov odpovídá predikát `findMostImportantKeyWord`, jež vrátí klíčové slovo s nejnižší prioritou, které ve větě nalezne, nebo `none`. Tento predikát je naimplementován metodou **akumulátoru**, kde druhý argument slouží jako akumulátor, ve kterém je předáváno zatím nejlepší nalezené klíčové slovo.

```Prolog
% cast implementace predikatu FindMostImportantKeyWord
% ukazuje predani obsahu akumulatoru do vysledku
findMostImportantKeyWord([],Res,Res):- nonvar(Res),!. %return keyWord found - Res has been already unified
findMostImportantKeyWord([],_,none). %no key word present in WordList
```

####Predikát `getResponse`

Za celý zbytek procesu budování odpovědi zodpovídá predikát `getResponse`. Ten dostane klíčové slovo a zjednodušenou vstupní větu. Tu postupně zkusí napasovat na všechny vzory vstupní věty příslušné danému klíčovému slovu. To provede pomocí predikátu `match`, jež naplní asociativní slovník slovy ze vstupní věty oindexovanými čísly v příslušném vstupním vzoru, se kterými byla slova *'zunifikována'* . Vzory vstupní věty reprezentuje predikát `pattern(Keyword,Pattern,PatternId, AdditionalInfo)`. Každý vstupní vzor má své `Id`, které je jedinečné v rámci vzorů příslušných jednomu klíčovému slovu a slouží k mapování mezi vstupními a výstupními vzory.

```Prolog
% priklad vstupnich vzoru
pattern(dream,[73,you,dream,1],1,_).
pattern(your,[73,your,42,X,1],2,X):-closeOne(X).
pattern(i,[73,i,1,you], 2,_).

```

Databázi výstupních vzorů (vzorů odpovědí) implementuje predikát `response`. Ten dostane klíčové slovo a `Id` vstupního vzoru a vrátí seznam příslušných výstupních vzorů.

```Prolog
% priklad sady vystupnich vzoru - response klauzule
% _ my _ closeOne() 1
response(your,2,[
    [tell,me,more,about,people,around,you,'.'],
    [who,else,you,know,1,?],
    [your,X,?],
    [what,else,comes,to,mind,when,you,think,of,your,X,?]
],X).
```

 O výběr konkrétního vzoru ze seznamu se stará predikát `getResponse` společně s dynamickým predikátem `toUse`. `Eliza` je naimplementována, aby postupně prostřídala všechny vzory z příslušného seznamu, je-li opakovaně konfrontována s podobnými dotazy. Predikát `toUse` má jednu klauzuli pro každou kombinaci klíčového slova a `Id` vstupního vzoru, ve které je uložen index toho vzoru odpovědi, který má být aktuálně použit. Po využití dané odpovědi je klauzule odstraněna prostřednictvím predikátu pro modifikaci programu `retract`, je vypočítán nový index a pomocí `assert` je přidána nová klauzule predikátu `toUse`.

```Prolog
toUse(KeyWord,PatternNum,LastUsedNum),
%forget old num of response pattern to use
retract(toUse(KeyWord, PatternNum,LastUsedNum)),
%pick response pattern coresponding to number    
nth0(LastUsedNum, ResponsePatternList, ResponsePattern),
%count new response index to be used in next iteration
length(ResponsePatternList, Len),
AnotherNum is ((LastUsedNum+1) mod Len),
%remember index 
assertz(toUse(KeyWord, PatternNum, AnotherNum)),
```

Počáteční hodnoty `toUse` klauzulí jsou na začátku běhu programu nainicializovány predikátem `prepareScript`.

Uvědomuji si, že četné užívání užívání predikátů pro modifikaci programu (`assert`, `retract`) běh programu zpomalí. Na druhou stranu předpokládám, že od softwaru, který v každé iteraci hlavní smyčky čeká na vstup od uživatele, se neočekává, že bude cílit na výkon.

Po vybrání adekvátního vzoru odpovědi  je znovu využit predikát `match`, tentokrát s již naplněným asociativním slovníkem. Tentokrát `match` do vzoru odpovědi substituuje slova z uživatelem napsané věty, co jeho první volání uložilo do asociativního slovníku.

#### Třídy slov

Některé vstupní vzory budou namatchovány  pouze, obsahuje-li vstupní věta slovo z dané třídy. Třídy sdružují slova podobného významu, která pasují na stejná místa ve větách. Je-li takový vzor namatchován, slovo z některé třídy slov je vráceno predikátem `pattern` jako `AdditionalInfo` a předáno predikátu `response`, který ho vloží na příslušnou pozici do výstupní věty. To umožňuje rozšířit množinu vstupních vět, na které vzor pasuje bez újmy na kvalitě odpovědi.

### Paměť

Eliza implementuje jednoduchou paměť. Pokud uživatel použije slovo, pro které je splněn predikát `important`, predikát `tryFindImportant` v druhé klauzuli `Eliza/1` uspěje a místo predikátu `getResponse` se použije `getMemoryResponse`. Tento predikát zkontroluje, zda je v programu již přítomna klauzule predikátu `mem(ImportantWord,SentenceInMemory)` s prvním argumentem odpovídajícím danému důležitému slovu. Pokud ano, je zapamatovaná věta vyjmuta z paměti (`retract`), použita v odpovědi (namatchovaná na speciální `response` pattern `mem`) a pro dané klíčové slovo je nově zapamatována věta, kterou uživatel právě zadal (`assert`). Jinak je věta pouze zapamatována a je zformulována standartní odpověď (`getResponse`). Eliza bere v potaz první důležité slovo, co se ve větě vyskytuje.

```Prolog
important(X):-family(X).
important(X):-closeOne(X).
important(hate).
important(love).
important(climbing).
important(birthday).
important(X):-day(X).
```

###Výpis výstupu

Na predikátu `reply`, která vypíše vybudovanou větu na výstup není nic pozoruhodného, snad jen že převede první písmeno prvního slova věty do upper case. To umožňuje vkládat části uživatelem napsané věty (jejíž všechna písmena byla při parsování převedena do lower case) i na začátek vybudované odpovědi bez újmy na tom, že každá slušná věta začíná velkým písmenem. Navíc to zbavuje mou programátorskou maličkost nutnosti kontrolovat, zda každý z nemála vzorů odpovědi začíná velkým písmenem.  

##Uživatelská dokumentace






