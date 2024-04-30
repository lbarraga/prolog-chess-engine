% Pieces.pl and board.pl
:- module(update_info, [update_info/5]).
:- use_module('../pieces.pl').
:- use_module('../pieces.pl').

% update_info(+Name, +Move, +Color, +Info, -NewInfo)
% Updates the info of the board after a move
% Name: Name of the piece that moved
% Move: Move that was made
% Color: Color of the piece that moved
% Info: Current info of the board
%     - Castling: Castling info
%     - EnPassant: En passant info
% NewInfo: New info of the board
update_info(Name, Move, Color, info(Castling, EnPassant), info(NewCastling, NewEnPassant)) :-
    update_en_passant(Name, Move, NewEnPassant),
    update_castling_color(Color, ColorCastling, NewColorCastling, Castling, NewCastling),
    update_castling(Name, Move, ColorCastling, NewColorCastling).


% ----------------------------------- Update castling -----------------------------------

% update_castling_color(+Color, +Old, +New, +White, -NewWhite)
% Updates the castling info of the board for the given color.
update_castling_color(white, Old, New, castling_info(Old, Black), castling_info(New, Black)).
update_castling_color(black, Old, New, castling_info(White, Old), castling_info(White, New)).

% update_castling(+Name, +Move, +Castling, -NewCastling)
% Disable long castle if the left rook moved.
update_castling(rook, move(From, _), Castling, NewCastling) :-
    disable_long_castle(Castling, NewCastling),
    rook_left(From), !.

% Disable short castle if the right rook moved.
update_castling(rook, move(From, _), Castling, NewCastling) :-
    disable_short_castle(Castling, NewCastling),
    rook_right(From), !.

% Disable all castling if the king moved.
update_castling(king, _, Castling, NewCastling) :-
    disable_long_castle(Castling, TempCastling),
    disable_short_castle(TempCastling, NewCastling), !.

% Do not change castling if the move did not involve the king or the rooks.
update_castling(_, _, Castling, Castling).

% Rook positions
rook_left((0, 0)). rook_left((7, 0)). rook_right((0, 7)). rook_right((7, 7)).

% disable_long_castle(+OldCastlingInfo, -NewCastlingInfo)
disable_short_castle((Long, _), (Long, no_short)).
disable_long_castle((_, Short), (no_long, Short)).


% ----------------------------------- Update en passant -----------------------------------

% update_en_passant(+Name, +Move, -NewEnPassant)
% Update en passant info with the square where the pawn can be captured (the empty square).
update_en_passant(pawn, move((R1, C), (R3, C)), (R2, C)) :-
    abs(R3 - R1) =:= 2,
    R2 is (R1 + R3) // 2, !.

% No en passant possible if the move was not a double move by a pawn.
% by setting this to no_ep, a possible en passant square of the previous move is disabled.
update_en_passant(_, _, no_ep).