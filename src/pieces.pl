black(Piece) :- member(Piece, ['r','n','b','q','k', 'p']).
white(Piece) :- member(Piece, ['R','N','B','Q','K', 'P']).

% Define piece names
name('r', rook).
name('R', rook).
name('n', knight).
name('N', knight).
name('b', bishop).
name('B', bishop).
name('q', queen).
name('Q', queen).
name('k', king).
name('K', king).
name('p', pawn).
name('P', pawn).

king(black, 'k').
king(white, 'K').

color(Piece, white) :- white(Piece).
color(Piece, black) :- black(Piece).

piece(Piece) :- black(Piece).
piece(Piece) :- white(Piece).

empty(' ').

%
pawn_direction(white, -1).
pawn_direction(black, 1).

pawn_start_row(white, (6, _)).
pawn_start_row(black, (1, _)).

opponent(white, black).
opponent(black, white).

could_capture(_, CapturedPiece) :- empty(CapturedPiece).
could_capture(CapturingPiece, CapturedPiece) :-
    color(CapturingPiece, CapturingColor),
    color(CapturedPiece, CapturedColor),
    opponent(CapturingColor, CapturedColor).


% Piece values
value(pawn, 1).
value(queen, 9).
value(rook, 5).
value(bishop, 3).
value(knight, 3).
value(king, 0).

