basic_piece_movement(king, (R1, C1), (R2, C2), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =< 1,
    abs(C2 - C1) =< 1.
