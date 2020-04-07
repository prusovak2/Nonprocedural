word(astante, a,s,t,a,n,t,e).
word(astoria, a,s,t,o,r,i,a).
word(baratto, b,a,r,a,t,t,o).
word(cobalto, c,o,b,a,l,t,o).
word(pistola, p,i,s,t,o,l,a).
word(statale, s,t,a,t,a,l,e).

solvePuzzle(V1,V2,V3,H1,H2,H3):-
    word(H1, _A1,A2,_A3,A4,_A5,A6,_A7),
    word(H2, _B1,B2,_B3,B4,_B5,B6,_B7),
    word(H3, _C1,C2,_C3,C4,_C5,C6,_C7),
    word(V1, _D1,A2,_D2,B2,_D3,C2,_D4),
    word(V2, _E1,A4,_E2,B4,_E3,C4,_E4),
    word(V3, _F1,A6,_F2,B6,_F3,C6,_F4),
    not(V1==H1), not(V2==H2), not(V3==H3).
