nahradJeden(X,Y,[X|H],[Y|H]). %nahrazeni
nahradJeden(X,Y,[H|T1],[H|T2]):- nahradJeden(X,Y,T1,T2).

nahradPrvni(X,Y,[X|H],[Y|H]). %nahrazeni
nahradPrvni(X,Y,[H|T1],[H|T2]):- nahradJeden(X,Y,T1,T2),!.

% rozdil(+Sez1, +Sez2, -Rozd) :- Rozd je seznam prvkù ze Sez1, které
% nejsou v Sez2
rozdil([],_,[]):-!.
rozdil([H|T1],X,[H|T2]):- \+member(H,X),rozdil(T1,X,T2),!.
% zjiti, ze to tam nema pridat, zakazu backtracing, aby to tam priste
% nezkusil pridat
rozdil([_|T1],X,T2):-  rozdil(T1,X,T2).

%TODO: zplosti(+Sez, -Zplosteny) :-
%     projde všechny seznamy v Sez a jejich prvky pøidá do Zplosteny
zplosti([],[]).
zplosti([X|Xs],[X|Rs]):- \+is_list(X),!,zplosti(Xs,Rs).
zplosti([H|T],Z):- zplosti(H,X),zplosti(T,Y), append(X,Y,Z).


% TODO: zplosti naprogramované pomoci if-then-else
% ?- zplosti2([1,[1,[],[1],2],3], X).
% X = [1, 1, 1, 2, 3].
zplosti2([],[]).
zplosti2([H|T], Vys):-
    (   is_list(H) -> zplosti2(H,Hz), zplosti2(T,Tz), append(Hz,Tz,Vys);
    zplosti2(T,Tz), Vys =[H|Tz]).

%%%%%%%%%%%%%%%%%%  ROZDILOVE SEZNAMY  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%prevod  rozdilovy ->normalni
na_normalni(Xs-[],Xs).
%prevod norm->rozdilovy
na_rozdilovy([],S-S).
na_rozdilovy([X|Xs],[X|S]-T):-na_rozdilovy(Xs,S-T).

%zretez rozdilove
zretez(A-B,B-C,A-C).

%enqueue se zretez
%enqueue(+X, +RozdilovySeznam, -RozdilovySeznamsXnaKonci)
enqueue(X,A-B,A-C):-zretez(A-B,[X|S]-S,A-C).

enqueue2(X,A-[X|S],A-S).

% TODO: dequeue(+RozdilovySeznam, -X, RozdilovySeznamBezPrvnihoPrvku)
% dequeue([1,2,3|S]-S, X, Y).
dequeue([X|Xs]-S,X,Xs-S).

%%%%%%%%%%%  TREES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% materiály: pøednáška 3
%
% mìjme strom reprezentovanı strukturou t(LevyPodstrom, HodnotaVrcholu, PravyPodstrom)
% list budeme reprezentovat jako t(nil, HodnotaListu, nil)
% pøíklad stromu: t(t(nil, 1, nil), 5, t(t(nil, 6, nil), 8, nil))
% TODO podle slidù: in(V, Strom) :- pravdivı pokud Strom obsah je V.

in(X,t(_,X,_)):-!.
in(X,t(Left,_,_)):- in(X,Left),!.
in(X,t(_,_,Right)):- in(X,Right),!.

%%%%%%%%%%%  GRAPHS  %%%%%%%%%%%%%%%%%%
% mìjme graf reprezentovanı jako graf(Vrcholy, Hrany)
graf([a, b, c, d, e, f], [a->b, b->c, c->a, c->d, e->f]).

% rozhraní:
vrchol(V) :- graf(Vrcholy,_), member(V,Vrcholy).
hrana(V1, V2):- graf(_,Hrany), member(V1->V2,Hrany).
% zkuste: ?- hrana(X,Y)
% urèitì lze pouívat i obecnìjší definici hrana(V1, V2, graf(Vrcholy,Hrany)):- member(h(V1,V2),Hrany).
% která má v argumentu vstupní graf. Pro pøehlednost ale zatím mùeme pouívat toto jednodušší rozhraní pro jeden konkrétní graf.

% TODO podle slidù: dfs(X, Y) :- pravdivı pokud existuje v grafu cesta z vrcholu X do vrcholu Y.
dfs(X,Y):-dfs(X,Y,[X]).
dfs(X,X,_):-!.
dfs(X,Z,Nav):-hrana(X,Y),\+member(Y,Nav),!,dfs(Y,Z,[Y|Nav]).

% TODO: trojuhelnik(X, Y, Z) :- pravdivı pokud existuje trojúhelník mezi tìmito vrcholy
trojuhelnik(X,Y,Z):-hrana(X,Y),hrana(Y,Z),hrana(Z,X).

% TODO: trojuhelniky(SeznamTroj) :- vrátí seznam všech trojúhelníkù
% (mùete pouít setof/3, viz pøednáška 4)

%trojuhelniky([X,Y,Z]):- trojuhelnik(X,Y,Z).

trojuhelniky(Sez):-setof([X,Y,Z], trojuhelnik(X,Y,Z),Sez).

% TODO: implementujte nulární existuje_cyklus, kterı bude pravdivı, pokud v grafu existuje cyklus
% hint: zkuste implementovat upravenı dfs2, kterı bude umìt hledat cestu z vrcholu V k sobì samému
% pak spuse dfs2 pro všechny vrcholy grafu.
% hint2: urèitì musíte upravit bázi dfs(X,X,_). tak, aby neskonèila triviálnì pro prázdnı seznam
dfs2(X,Y):-dfs2(X,Y,[X]).
dfs2(X,X,[_|_]):-!.
dfs2(X,Z,Nav):-hrana(X,Y),\+member(Y,Nav),!,dfs2(Y,Z,[Y|Nav]).

existuje_cyklus :- vrchol(V),dfs2(V,V),!.

% TODO podle slidù: implementujte breadth first search
bfs(Start, Cil, Cesta):-bfs1([Start],Cil,RevCesta), reverse(RevCesta,Cesta).

%bfs1(fronta, cil, revCesta)

bfs1([Xs|_], Cil, Xs):- Xs=[Cil|_].
bfs1([[X|Xs]|Xss], Cil, CestaR):-findall([Y,X|Xs],(hrana(X,Y),\+member(Y,[X|Xs])),NoveCesty),append(Xss,NoveCesty,NovaFronta),!,bfs1(NovaFronta,Cil,CestaR).




