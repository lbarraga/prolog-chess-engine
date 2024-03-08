
% ---------------------------------------------------------------------------------------------------------
%                                                   Moves 
% ---------------------------------------------------------------------------------------------------------
% In this module, the possible move axes of every piece are defined. This is not with respect to a board,
%           where the set of possible moves is reduced if other pieces are in the way. 
% ---------------------------------------------------------------------------------------------------------



% -------------------------------------------------- Rook -------------------------------------------------

reach(rook, (R, _), Axis) :- row_coordinates(R, Axis).
reach(rook, (_, C), Axis) :- column_coordinates(C, Axis).

% -------------------------------------------------- Rook -------------------------------------------------

positive_integer(1).
positive_integer(X) :- positive_integer(Y), X is Y + 1.

sum_of_two(N, A, B) :-
    positive_integer(A),
    positive_integer(B),
    A + B =:= N,
    A =< B.
