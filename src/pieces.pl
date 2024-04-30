:- module(pieces, [
    opponent/2,
    king/2,
    black/1,
    white/1,
    name/2,
    color/2,
    piece/1,
    empty/1,
    pawn_direction/2,
    pawn_start_row/2,
    value/2
]).

% == General piece definitions ==
% The predicates speak for themselves

black('r').
black('n').
black('b').
black('q').
black('k').
black('p').

white('R').
white('N').
white('B').
white('Q').
white('K').
white('P').

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

% Piece values
value(pawn, 1).
value(queen, 9).
value(rook, 5).
value(bishop, 3).
value(knight, 3).
value(king, 0).

