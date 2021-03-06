%%%%%%%% BIT FLIP  %%%%%%
%
%returns list of same lenght containig only zero bits
toZeroes([],[]).
toZeroes([_|List],[0|Zeroes]):-toZeroes(List,Zeroes).

%flips one bit In and returns it in Out
flipBit(In, Out):-In==0, Out is 1,!.
flipBit(In, Out):-In==1, Out is 0,!.

%flip 3 neightbiring inner bits
flip3inner([X1,X2,X3|Rest],[Y1,Y2,Y3|Rest]):-flipBit(X1,Y1),flipBit(X2,Y2),flipBit(X3,Y3).
flip3inner([],[]).
flip3inner([X|Rest],[X|Returned]):-flip3inner(Rest,Returned).

%flip3inner([X|Rest],[X|Returned]):-flip3inner(Rest,Returned).
% flip3inner([X1,X2,X3],[Y1,Y2,Y3]):-flipBit(X1,Y1),flipBit(X2,Y2),flipBit(X3,Y3).

flipLast([Last],[LastFlipped]):-flipBit(Last,LastFlipped),!.
flipLast([X|Xs],[X|Ys]):-flipLast(Xs,Ys).

flipLastTwo([X1,X2],[Y1,Y2]):-flipBit(X1,Y1),flipBit(X2,Y2),!.
flipLastTwo([X|Xs],[X|Ys]):-flipLastTwo(Xs,Ys).

flip3outer([X|Res],[Y|Fr]):-flipBit(X,Y),flipLastTwo(Res,Fr).
flip3outer([X1,X2|Rest],[Y1,Y2|Fr]):-flipBit(X1,Y1),flipBit(X2,Y2),flipLast(Rest,Fr). %flip two leading and one trailing bit

flip3(In, Out):-flip3inner(In, Out), In\==Out.
flip3(In, Out):-flip3outer(In, Out).


%%%%%%%   BFS   %%%%%%%%%%%

%hrana(a, b).
%hrana(b, c).
%hrana(a, c).
%hrana(c, d).

hrana(X,Y):-flip3(X,Y).

% bfs(Start, Cil, Cesta) :- bfs1([[Start]], Cil, CestaRev),
% reverse(CestaRev, Cesta).
% bfs1([Xs|_], Cil, Xs) :- Xs = [Cil|_],!. %thanks to ! returns only the
% first path found
%bfs1([[X|Xs]|Xss], Cil, CestaR) :-
%    findall([Y,X|Xs], (hrana(X,Y), \+ member(Y,[X|Xs])), NoveCesty),
%    append(Xss, NoveCesty, NovaFronta), !,
%    bfs1(NovaFronta, Cil, CestaR).

bfs(Start, Cil, Cesta) :- bfs1([[Start]], Cil, CestaRev,[Start]),
    reverse(CestaRev, Cesta),!.
bfs1([Xs|_], Cil, Xs,_) :- Xs = [Cil|_],!. %thanks to ! returns only the
% first path found
bfs1([[X|Xs]|Xss], Cil, CestaR,Nav) :-
    findall([Y,X|Xs], (hrana(X,Y), \+ member(Y,[X|Xs]),
    \+member(Y,Nav)), NoveCesty),
    append(Xss, NoveCesty, NovaFronta),!,
    bfs1(NovaFronta, Cil, CestaR,[X|Nav]).

flipBits(Start,Path):-toZeroes(Start,Zeroes),bfs(Start,Zeroes,Path).
