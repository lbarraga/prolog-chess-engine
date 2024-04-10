basic_piece_movement(bishop, From, To, Board) :- in_sight(diagonal, From, To, Board).
basic_piece_movement(bishop, From, To, Board) :- in_sight(anti_diagonal, From, To, Board).