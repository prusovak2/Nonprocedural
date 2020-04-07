% NEDETERMINISMUS

% soucetPodmnoziny(+SeznamCisel, +SoucetPodmnoziny, -PodmnozinaCisel).
% ?- soucetPodmnoziny([1,2,4,6], 6, Podmn).

soucetPodmnoziny([],0,[]).
soucetPodmnoziny([H|T],S,[H|Vys]):- Snove is S-H, soucetPodmnoziny(T,Snove,Vys).
soucetPodmnoziny([_|T],S,Vys):- soucetPodmnoziny(T,S,Vys).


% TODO: generov�n� v�raz�, m�jme seznam ��sel, predik�t vzorec(Seznam, Vz) bude spln�n, pokud v prom�nn�
%	    Vz bude v�raz sestaven� z ��sel v seznamu (pou�ijte append/3 a jeho nedeterministickou
%	    schopnost rozd�lovat seznam na levou a pravou ��st):

vzorec([X],X).
vzorec(Sez,Lv+Pv):-append(L,P,Sez), L\=[], P\=[],vzorec(L,Lv),vzorec(P,Pv).
vzorec(Sez,Lv-Pv):-append(L,P,Sez),L\=[], P\=[],vzorec(L,Lv),vzorec(P,Pv).
% append takto pouzity mi nedeterministicky rozdeli seznam na vsechny
% mozne 'poloviny' - prvni a druhou cast
% jen nastineni reseni, chybi pocatecni pripad a jeste neco?


graf([a, b, c, d, e, f], [a->b, b->c, c->a, c->d, e->f]).
vrchol(V) :- graf(Vrcholy,_), member(V,Vrcholy).
hrana(V1, V2):- graf(_,Hrany), member(V1->V2,Hrany).

trojuhelnik(X,Y,Z):-hrana(X,Y),hrana(Y,Z),hrana(Z,X).

% TODO: jezdcova proch�zka pomoc� DFS (nejsp� n�m pro opravdov� proch�zky nedob�hne, ale to nevad�)
% pot�ebujeme predik�t skok(p(X,Y), p(X2,Y2)), kter� je splniteln� pro v�echny validn� skoky z pozice p(X,Y).
% ?- member(X, [0,1,2]), member(Y, [0,1,2]), cestaKT(p(X,Y), p(X,Y), C, [])

validni(p(X,Y)) :- X >= 0, Y >= 0, X =< 5, Y =< 5.
skok(p(X,Y), p(X2,Y2)) :- X2 is X+1, Y2 is Y+2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-1, Y2 is Y+2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X+1, Y2 is Y-2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-1, Y2 is Y-2, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X+2, Y2 is Y+1, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-2, Y2 is Y+1, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X+2, Y2 is Y-1, validni(p(X2, Y2)).
skok(p(X,Y), p(X2,Y2)) :- X2 is X-2, Y2 is Y-1, validni(p(X2, Y2)).

dfs2(X,Y):-dfs2(X,Y,[X]).
dfs2(X,X,[_|_]):-!.
dfs2(X,Z,Nav):-skok(X,Y),\+member(Y,Nav),!,dfs2(Y,Z,[Y|Nav]).


