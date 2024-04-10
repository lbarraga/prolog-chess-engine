castle(Type, Color, Board, Info, NewBoard, _) :-  % TODO info moet veranderen.
    can_castle(Type, Color, Board, Info),
    between_squares(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos),
    move(KingPos, TwoNextToKingPos, Board, TempBoard),
    move(RookPos, OneNextToKingPos, TempBoard, NewBoard).


can_castle(Type, Color, Board, info(CanCastleShort, CanCastleLong)) :-
    \+ king_in_check(Board, Color),
    info_ok_for_castling(Type, CanCastleShort, CanCastleLong),
    between_squares(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos),
    uber_can_move(RookPos, OneNextToKingPos, Color, Board),             % Rook could move next to king.
    uber_move(KingPos, OneNextToKingPos, Color, Board, TempBoard),      % King could move one square closer to rook.
    uber_move(OneNextToKingPos, TwoNextToKingPos, Color, TempBoard, _). % King could move two squares closer to rook.

%                                     King   King+1  King+2   Rook
between_squares(white, long_castle,  (7, 4), (7, 3), (7, 2), (7, 0)).
between_squares(white, short_castle, (7, 4), (7, 5), (7, 6), (7, 7)).
between_squares(black, long_castle,  (0, 4), (0, 3), (0, 2), (0, 0)).
between_squares(black, short_castle, (0, 4), (0, 5), (0, 6), (0, 7)).

info_ok_for_castling(short_castle, can_castle_short, _).
info_ok_for_castling(long_castle, _, can_castle_long).