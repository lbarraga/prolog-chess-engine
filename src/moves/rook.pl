basic_piece_movement(rook, From, To, Board) :- in_sight(row, From, To, Board).
basic_piece_movement(rook, From, To, Board) :- in_sight(column, From, To, Board).