:- module(parse_move, [top_move/2]).

promoting_piece --> bishop; knight; rook; queen.    % A piece which can be promoted to.
piece --> promoting_piece; king.                    % A general piece on the board.


% Define individual characters for each chess piece.
king   --> "K".
bishop --> "B".
knight --> "N".
queen  --> "Q".
rook   --> "R".

% Define a chess coordinate
file --> "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h".
rank --> "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8".

coordinate --> file, rank.

% Define specials
takes --> "x".
promotes --> "=".
check --> "+".
checkmate --> "#".
short_castle --> "O-O".
long_castle --> "O-O-O".
disambiguation --> file; rank; coordinate.


% Unified rule for the destination part of a move, which includes the coordinate and an optional promotion
destination --> coordinate, optional_promotion.

% Generalized move rules
move --> piece, optional_disambiguation, optional_takes, destination.   % General piece move.
move --> file, takes, destination.                                      % Pawn capture.
move --> destination.                                                   % Pawn move.
move --> short_castle; long_castle.                                     % Special move.

% Define components of moves
optional_disambiguation --> disambiguation; [].
optional_takes --> takes; [].
optional_promotion --> promotes, promoting_piece; [].

% Post-move conditions
optional_check_or_checkmate --> check; checkmate; [].

% Top-level rule for moves including post-move conditions
top_move --> move, optional_check_or_checkmate.
