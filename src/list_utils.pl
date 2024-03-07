:- module(list_utils, [replace2D/4, split/4]).

% Replace the item at index `Index` with `Replacement` in the first list to get the second list
replace(0, Replacement, [_|T], [Replacement|T]).
replace(Index, Replacement, [H|T1], [H|T2]) :-
    Index > 0,
    K is Index - 1,
    replace(K, Replacement, T1, T2).


replace2D((R, C), Replacement, Matrix, NewMatrix) :-
    nth0(R, Matrix, Row),
    replace(C, Replacement, Row, NewRow),
    replace(R, NewRow, Matrix, NewMatrix).


split(Index,List,Left,Right) :-
   length(Left,Index),
   append(Left,[_|Right],List).