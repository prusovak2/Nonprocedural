pridejz(X,S,[X|S]).

memberm(X,[X|S]).
memberm(X, [X|S]):-X\=A, member(X,S).

acka_becka([],[]).
acka_becka([a|X],[b|Y]):-acka_becka(X,Y).

zip([],[],[]).
zip([X|Xs],[Y|Ys],[X,Y|Zs]):- zip(Xs,Ys,Zs).

prefix([],_).
prefix([P|Ps],[A|As]):-prefix(Ps,As).

% loupu prvni prvky ze seznamu dokud nemam prazdny seznam, ke kteremu
% umim zbytek druheho seznamu pripojit podle prvni klauzule
append([],X,X).
append(X, [A|Y], [A|Z]):-append(X,Y,Z).

pridejk(X,[],[X]).
pridejk(X,[Y|Ys],[Y|Z]):-pridejk(X,Ys,Z).

otoc([],[]).
otoc([X|Xs],Y):-otoc(Xs,Z), pridejk(X,Z,Y).

%otoceni s akumulatorem(vstupniS, akumulator, otocenyS)
otocAk([],OtSez,OtSez).
otocAk([H|T],A,OtSez):-otocAk(T,[H|A],OtSez).

vymaz(X,[X|Xs], Xs).
vymaz(Co, [Y|Ys], [Y|Vys]):-vymaz(Co, Ys, Vys).

prostredni(X,[X]).
prostredni(A, [_|Xs]):-append(Ys, [_],Xs),prostredni(A,Ys).


