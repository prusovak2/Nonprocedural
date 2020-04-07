%peanova aritmetika- naslednik
%rekurzivni definice relace naslednika
numeral(0).
numeral(s(X)) :- numeral(X).

lessThan(0,s(X)):-numeral(X).
lessThan(s(X),s(Y)):-lessThan(X,Y).

lessEq(0,X):-numeral(X).
lessEq(s(X),s(Y)):-lessEq(X,Y).

add(0,X,X):-numeral(X).
add(s(X),Y,s(R)):-add(X,Y,R).

subtract(X,Y,R):-add(Y,R,X).
%subtract(X,Y,R):-add(X,R,Y).

multi(0,X,0):-numeral(X).
multi(s(X),Y,R):-multi(X,Y,R2),add(R2,Y,R).

divide(_,0,_,_):-!, fail.
divide(0,_,0,0):-!.
divide(X,Y,Res,Rem):- div(X,Y,0,Res,Rem).

div(X,Y,Res,Res,X):- lessThan(X,Y).
div(X,Y,T,Res,Rem) :-
    lessEq(Y,X), subtract(X,Y,X1), add(T,s(0),T1), div(X1,Y,T1,Res,Rem).






