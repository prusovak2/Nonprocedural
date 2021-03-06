vzorec([X],X).
vzorec(Sez,Lv+Pv):-append(L,P,Sez), L\=[], P\=[],vzorec(L,Lv),vzorec(P,Pv).
vzorec(Sez,Lv-Pv):-append(L,P,Sez),L\=[], P\=[],vzorec(L,Lv),vzorec(P,Pv).
% append takto pouzity mi nedeterministicky rozdeli seznam na vsechny
% mozne 'poloviny' - prvni a druhou cast
% jen nastineni reseni, chybi pocatecni pripad a jeste neco?

%gen([X],X).
% gen(Sez,V):-append(L,P,Sez), L\=[], P\=[],gen(L,Lv),gen(P,Pv),V
% =..[+,Lv,Pv].
% gen(Sez,V):-append(L,P,Sez),L\=[], P\=[],gen(L,Lv),gen(P,Pv), V
% =..[-,Lv,Pv].

gen([X],_,X).
gen(Sez,OpList,V):-
    append(L,P,Sez),
    L\=[], P\=[],
    gen(L,OpList,Lv),gen(P,OpList,Pv),
    tryAll(OpList,Lv,Pv,V).

tryAll([Operator|_],Lv,Pv,Expr):- Expr=..[Operator,Lv,Pv].
tryAll([_|T],Lv,Pv,Expr):-tryAll(T,Lv,Pv,Expr).

%s(*,X,1,X).
%s(*,1,X,X).
s(*,X,Y,Z):-number(X),number(Y), Z is X*Y.
s(+,X,Y,Z):-number(X),number(Y), Z is X+Y.
s(-,X,Y,Z):-number(X),number(Y), Z is X-Y.
s(/,X,Y,Z):-number(X),number(Y), Y=\=0, Z is X/Y.

safe_is(V,V):-atomic(V),!.
safe_is(Expr, Res):-
    Expr =..[Op, Lv,Pv],
    safe_is(Lv,Lr),
    safe_is(Pv,Pr),
    s(Op,Lr,Pr,Res).

gen_result(Vars,Res,Expr):-gen(Vars,[+,-,*,/],Expr),safe_is(Expr,Res).




