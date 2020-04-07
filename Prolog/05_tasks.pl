%diff/3 (+List1, +[List2], -[Difference]). Where Difference is list of items contained in List1 but not in List2.
diff([], _, []).
diff([Head|Tail], List2, [Head|Difference]):-
    \+ member(Head, List2), !, diff(Tail, List2, Difference).
diff([Head|Tail], List2, Difference):-
    member(Head, List2), diff(Tail, List2, Difference).


% flatten/2 (+Items, -List) takes Items wich may contain lists returns only
% items in all lists given and sole members of first Items
flatten([], []).
flatten([Head|Tail], [Head|Result]):-
    \+ is_list(Head), !, flatten(Tail, Result).
flatten([Head|Tail], Result):-
    is_list(Head), flatten(Head, FlatHead), flatten(Tail, FlatTail),
    append(FlatHead, FlatTail, Result).

flatten_ifte([], []).
flatten_ifte([Head|Tail], Result):-
    (   is_list(Head)
    ->  flatten_ifte(Head|FlatHead),
        flatten_ifte(Tail|FlatTail),
        append(FlatHead, FlatTail, Result)
    ;   Result = [Head|FlatTail],
    	flatten_ifte(Tail, FlatTail)
    ).


% replace_one/4 (+X, +Y, List, Result):- replace one X in list with Y
replace_one(X, Y, [X|Tail], [Y|Tail]).
replace_one(X, Y, [Head|Tail], [Head|Result]):-
    replace_one(X, Y, Tail, Result).


% replace_first/4 (+X, +Y, List, Result):- replace first X in list with Y
replace_first_cut(X, Y, [X|Tail], [Y|Tail]):- !.
replace_first_cut(X, Y, [H|Tail], [H|Result]):-
    replace_first_cut(X, Y, Tail, Result).

replace_first_once(X, Y, [X|Tail], [Y|Tail]).
replace_first_once(X, Y, [H|Tail], [H|Result]):-
    once(replace_first_once(X, Y, Tail, Result)).


% to_normal_list/2 (+X-[], -X):- gets diff list, returns plain list
to_normal_list(List-[], List).

% to_diff_list/2 (+List, -List-[]): gets list, returns diff list
to_diff_list([], S-S).
to_diff_list([Head|Tail], [Head|Tail]-S) :- to_diff_list(Tail, Tail-S).

% appendDiff/3 (+A-B, +C-D, -A-D) :- C is appended to A
% appendDiff(A-B, C-D, A-D):- B = C. This is how it works...

appendDiff(A-B, B-D, A-D).

% rotate(+A, -B):- B is A rotated by one item to the left
rotate([Head|Tail], Result) :- append(Tail, [Head], Result).

% rotateDiff(+A-S, -B-S):- B is A rotated by one item to the left
rotateAppendDiff([Head|Tail]-S1, Res-S2) :- appendDiff(Tail-S1, [Head|S3]-S3, Res-S2).

rotateDiff([Head|Tail]-[Head|S], Tail-S).
