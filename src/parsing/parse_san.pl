:- module(parse_san, [san/4]).

promoting_piece(P) --> bishop(P); knight(P); rook(P); queen(P).     % A piece which can be promoted to.
piece(P) --> promoting_piece(P); king(P).                           % A general piece on the board.


% Define individual characters for each chess piece.
king(king)   --> "K".
bishop(bishop) --> "B".
knight(knight) --> "N".
queen(queen)  --> "Q".
rook(rook)   --> "R".

% Define ranks and files. These are converted to 0-indexed integers, representing the row and column of the board.
% (0, 0) is the top-left corner of the board.
file(Col) --> {member(F, ["a", "b", "c", "d", "e", "f", "g", "h"]), Col is F - 97}, F.
rank(Row) --> {member(R, ["1", "2", "3", "4", "5", "6", "7", "8"]), Row is 56 - R}, R.

coordinate(co(Row, Col)) --> file(Col), rank(Row).

% Define specials
takes --> "x".
promotes --> "=".
check --> "+".
checkmate --> "#".
short_castle --> "O-O".
long_castle --> "O-O-O".

% Disambiguation can be a file, rank, or a coordinate.
disambiguation(file(DisAmb)) --> file(DisAmb).
disambiguation(rank(DisAmb)) --> rank(DisAmb).
disambiguation(DisAmb) --> coordinate(DisAmb).

% Unified rule for the destination part of a move, which includes the coordinate and an optional promotion
destination(Co, Prom) --> coordinate(Co), optional_promotion(Prom).

% Generalized move rules
move(ply(P, Takes, Co, D, Prom)) -->
    piece(P), optional_disambiguation(D), optional_takes(Takes), destination(Co, Prom). % General piece move.
move(ply(pawn, takes, Co, file(F), Prom)) --> file(F), takes, destination(Co, Prom).   % Pawn capture.
move(ply(pawn, no_takes, Co, none, Prom)) --> destination(Co, Prom).                   % Pawn move.
move(castle(short)) --> short_castle.                                                   % short castle.
move(castle(long)) --> long_castle.                                                     % long castle.

% == Define components of moves ==

optional_disambiguation(DisAmb) --> disambiguation(DisAmb).
optional_disambiguation(none) --> [].

optional_takes(takes) --> takes.
optional_takes(no_takes) --> [].

optional_promotion(P) --> promotes, promoting_piece(P).
optional_promotion(no_promotion) --> [].

% Post-move conditions
optional_check_or_checkmate(checkmate) --> checkmate.
optional_check_or_checkmate(check) --> check.
optional_check_or_checkmate(no_check) --> [].

% Top-level rule for moves including post-move conditions (Check, Checkmate, or nothing)
san(Move, Check) --> move(Move), optional_check_or_checkmate(Check).