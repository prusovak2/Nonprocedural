%aritmetika
nadruhou(X,Y):-Y is X*X.

jeSudy(X):- 0 is  X mod 2.

jeSudyPole([]).
jeSudyPole([X|Y]):-jeSudy(X), jeSudyPole(Y).

mocnina(_,0,1):-!.
mocnina(X,1,X):-!.
mocnina(X,N,V):-
    N>1,
    Nminus is N-1,
    mocnina(X,Nminus,Vmensi),
    V is  Vmensi*X.

maximum(X,Y,Max):-X>=Y, Max is X,!.
maximum(X,Y,Max):-Y>X, Max is Y,!.

delkaSez([],0).
delkaSez([_|Sez], Len):- delkaSez(Sez, LenM), Len is LenM+1.

maxSez([X],X).
maxSez([Y|Ys],Max):- maxSez(Ys, Ms), maximum(Ms,Y,Max).

%prace s akumulatorem
delkaAk([],Vys,Vys). %ve vysledku mam uz spocitany vysledek, vratim ho
delkaAk([_|T],Vys,Ak):-
    VetsiAk is Ak +1,
    delkaAk(T, Vys, VetsiAk).

maxAk([],Max,Max).
maxAk([X|Xs],Max, Ak):-
    maximum(X,Ak,DalsiAk),
    maxAk(Xs,Max, DalsiAk).

%volani maxAk zvenku, inicializuje akumutator prvnim prvkem seznamu
maxAkPublic([H|T],Max):- maxAk(T,Max,H).

%zabalit akumulator prisup
fold(_,[],Vys,Vys). %base case
fold(Pred,[H|Sez],Ak,Vys):-
    call(Pred, H, Ak, DalsiAk),
    fold(Pred,Sez, DalsiAk, Vys).

plus(X,Y,Z):- Z is X+Y.

hlavy_tela([],[],[]).
hlavy_tela(Hs,Ts):-
