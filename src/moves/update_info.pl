% Pieces.pl and board.pl
:- module(update_info, [update_info/5]).
:- use_module('../pieces.pl').
:- use_module('../pieces.pl').

update_info(Name, Move, Color, info(Castling, EnPassant), info(NewCastling, NewEnPassant)) :-
    update_en_passant(Name, Move, EnPassant, NewEnPassant),
    update_castling_color(Color, ColorCastling, NewColorCastling, Castling, NewCastling),
    update_castling(Name, Move, ColorCastling, NewColorCastling).


% ----------------------------------- Update castling -----------------------------------

update_castling_color(white, Old, New, castling_info(Old, Black), castling_info(New, Black)).
update_castling_color(black, Old, New, castling_info(White, Old), castling_info(White, New)).


update_castling(rook, move(From, _), Castling, NewCastling) :-
    disable_long_castle(Castling, NewCastling),
    rook_left(From), !.

update_castling(rook, move(From, _), Castling, NewCastling) :-
    disable_short_castle(Castling, NewCastling),
    rook_right(From), !.

update_castling(king, _, Castling, NewCastling) :-
    disable_long_castle(Castling, TempCastling),
    disable_short_castle(TempCastling, NewCastling), !.

update_castling(_, _, Castling, Castling).

rook_left((0, 0)). rook_left((7, 0)). rook_right((0, 7)). rook_right((7, 7)).

disable_short_castle((Long, _), (Long, no_short)).
disable_long_castle((_, Short), (no_long, Short)).


% ----------------------------------- Update en passant -----------------------------------

% En passant
update_en_passant(pawn, move((R1, C), (R3, C)), _, (R2, C)) :-
    abs(R3 - R1) =:= 2,
    R2 is (R1 + R3) // 2, !.

update_en_passant(_, _, _, no_ep).