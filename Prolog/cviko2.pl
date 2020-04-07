%peanova aritmetika- naslednik
%rekurzivni definice relace naslednika
numeral(0).
numeral(s(X)) :- numeral(X).

zdvoj(0,0).
zdvoj(s(X), s(s(Y))) :- zdvoj(X,Y).

%sudy(0).
%sudy(s(s(X))) :- sudy(X).

sudy(X) :- zdvoj(_,X).

%soucet(0,0,0).
soucet(X,0,X).
soucet(X,s(Y),s(Z)) :- soucet(X,Y,Z).
%soucet(s(MenxiX),Y,s(MensiZ)) :- soucet(MensiX,Y,MensiZ).

soucin(_,0,0).
soucin(0,_,0).
soucin(s(X),Y,Z2) :-soucet(Z1,Y,Z2), soucin(X,Y,Z1).
