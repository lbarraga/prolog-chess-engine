basic_piece_movement(knight, (R1, C1), (R2, C2), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R1 - R2) =:= 1,
    abs(C2 - C1) =:= 2.

basic_piece_movement(knight, (R1, C1), (R2, C2), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =:= 2,
    abs(C2 - C1) =:= 1.