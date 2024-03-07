white(Piece) :- member(Piece, ['r','n','b','q','k', 'p']).
black(Piece) :- member(Piece, ['R','N','B','Q','K', 'P']).

piece(Piece) :- black(Piece).
piece(Piece) :- white(Piece).

empty(' ').