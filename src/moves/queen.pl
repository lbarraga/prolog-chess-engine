basic_piece_movement(queen, From, To, Board) :- can_move(bishop, From, To, Board).
basic_piece_movement(queen, From, To, Board) :- can_move(rook, From, To, Board).