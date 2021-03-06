% BFS.pl
%graf([a,b,c,d], [h(a,b), h(b,c), h(c,a), h(c,d)]).

vrchol(V, graf(Vrcholy, _)):- member(V, Vrcholy).
hrana(V1, V2, graf(_, Hrany)) :- member(h(V1, V2), Hrany).
hrana(V1, V2) :- hrana(V1,V2, graf([a,b,c,d], [h(a,b), h(b,c), h(c,a), h(c,d)])).

bfs(Start, Cil, Cesta) :- bfs1([[Start]], Cil, CestaRev), reverse(CestaRev, Cesta).
bfs1([Xs|_], Cil, Xs) :- Xs = [Cil|_].
bfs1([[X|Xs]|Xss], Cil, CestaR) :-
    findall([Y,X|Xs], (hrana(X,Y), \+ member(Y,[X|Xs])), NoveCesty),
    append(Xss, NoveCesty, NovaFronta), !,
    bfs1(NovaFronta, Cil, CestaR).

% priklady: bfs(c, b, Cesta).
%           bfs(d, a, Cesta).
%           bfs(a, d, Cesta).
%
% bfs(Start, Cil, Cesta) :- bfs1([[Start]], Cil, CestaRev,[Start]),
% reverse(CestaRev, Cesta).
%bfs1([Xs|_], Cil, Xs,_) :- Xs = [Cil|_].
%bfs1([[X|Xs]|Xss], Cil, CestaR,Nav) :-
%    findall([Y,X|Xs], (hrana(X,Y), \+ member(Y,[X|Xs]),
%    \+member(Y,Nav)), NoveCesty),
%    append(Xss, NoveCesty, NovaFronta), !,
%    bfs1(NovaFronta, Cil, CestaR,[Y|Nav]).
%
%
%    bfs(Start, Cil, Cesta) :- bfs1([[Start]], Cil, CestaRev,[Start]),
%    reverse(CestaRev, Cesta),!.
%bfs1([Xs|_], Cil, Xs,_) :- Xs = [Cil|_],!.
%bfs1([[X|Xs]|Xss], Cil, CestaR,Nav) :-
%    findall([Y,X|Xs], (hrana(X,Y), \+ member(Y,[X|Xs]),
%    \+member(Y,Nav)), NoveCesty),
%   findall(Y, (hrana(X,Y), \+ member(Y,[X|Xs]),
%    \+member(Y,Nav)), NoveY),
%    append(Xss, NoveCesty, NovaFronta),
%    append(Nav,NoveY,NoveNav),!,
%   bfs1(NovaFronta, Cil, CestaR,NoveNav).

