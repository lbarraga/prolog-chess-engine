legal_move(castle(Type), State, NewState) :-
    castle(Type, State, NewState).

legal_move(promotion(From, To, Promotion), State, NewState) :-
    legal_move(move(From, To), State, NewState).

legal_move(move(From, To), State, NewState) :-
    basic_move_unsafe(move(From, To), State, NewState),
    \+ king_in_check(NewState).


basic_move_unsafe(Move, State, NewState) :-
    opposing_color_or_empty(Move, State),
    basic_piece_move_unsafe(Name, Move, State, NewState).

opposing_color_or_empty(move(From, To), state(Board, _, Color)) :-
    get(From, Board, FromPiece),


basic_piece_move_unsafe(Name, Move, state(Board, Info, ToMove), state(NewBoard, NewInfo, Opponent)) :-
    opponent(ToMove, Opponent),
    piece_rule(Name, Move, Board),
    move_unsafe(Name, Move, Board, NewBoard),
    update_info(Name, Move, Info, NewInfo).

rook_left((0, 0)). rook_left((7, 0)). rook_right((0, 7)). rook_right((7, 7)).

update_info(rook, move(From, _), info(_, Short, _), info(no_long, Short, no_ep)) :- rook_left(From).
update_info(rook, move(From, _), info(Long, _, _), info(Long, no_short, no_ep)) :- rook_right(From).

update_info(bishop, _, info(Long, Short, _), info(Long, Short, no_ep)).
update_info(queen,  _, info(Long, Short, _), info(Long, Short, no_ep)).
update_info(knight, _, info(Long, Short, _), info(Long, Short, no_ep)).
update_info(king,   _, _, info(no_long, no_short, no_ep)).

update_info(pawn, move((R1, C), (R3, C)), info(Long, Short, _), info(Long, Short, (R2, C))) :-
    abs(R3 - R1) =:= 2,
    R2 is (R1 + R3) // 2, !.

update_info(pawn, _, info(Long, Short, _), info(Long, Short, no_ep)).





castle(Type, state(Board, Info, Color), state(NewBoard, info(no_long, no_short, no_ep), Opponent)) :-
    opponent(Color, Opponent),
    can_castle(Type, state(Board, Info, Color)),
    between_squares(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos),
    move_unsafe(KingPos, TwoNextToKingPos, Board, TempBoard),
    move_unsafe(RookPos, OneNextToKingPos, TempBoard, NewBoard).


can_castle(Type, state(Board, info(Long, Short, _), Color)) :-
    \+ king_in_check(state(Board, _, Color)),
    info_ok_for_castling(Type, Long, Short),
    between_squares(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos),
    uber_can_move(RookPos, OneNextToKingPos, Color, Board),             % Rook could move next to king.
    uber_move(KingPos, OneNextToKingPos, Color, Board, TempBoard),      % King could move one square closer to rook.
    uber_move(OneNextToKingPos, TwoNextToKingPos, Color, TempBoard, _). % King could move two squares closer to rook.

%                                     King   King+1  King+2   Rook
between_squares(white, long_castle,  (7, 4), (7, 3), (7, 2), (7, 0)).
between_squares(white, short_castle, (7, 4), (7, 5), (7, 6), (7, 7)).
between_squares(black, long_castle,  (0, 4), (0, 3), (0, 2), (0, 0)).
between_squares(black, short_castle, (0, 4), (0, 5), (0, 6), (0, 7)).

info_ok_for_castling(short, long, _).
info_ok_for_castling(long, _, short).



king_in_check(state(Board, _, Color)) :-
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    basic_move_unsafe(move(_, KingCo), state(Board, _, OpponentColor), _).




piece_rule(rook, From, To, Board) :- in_sight(row, From, To, Board).
piece_rule(rook, From, To, Board) :- in_sight(column, From, To, Board).

% ------------------------------------------------- Bishop -----------------------------------------------

piece_rule(bishop, From, To, Board) :- in_sight(diagonal, From, To, Board).
piece_rule(bishop, From, To, Board) :- in_sight(anti_diagonal, From, To, Board).

% -------------------------------------------------  Queen  -----------------------------------------------

piece_rule(queen, From, To, Board) :- can_move(bishop, From, To, Board).
piece_rule(queen, From, To, Board) :- can_move(rook, From, To, Board).

% -------------------------------------------------  Knight  -----------------------------------------------

piece_rule(knight, (R1, C1), (R2, C2), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R1 - R2) =:= 1,
    abs(C2 - C1) =:= 2.

piece_rule(knight, (R1, C1), (R2, C2), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =:= 2,
    abs(C2 - C1) =:= 1.

% -------------------------------------------------  King  -----------------------------------------------

piece_rule(king, (R1, C1), (R2, C2), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =< 1,
    abs(C2 - C1) =< 1.

% -------------------------------------------------  Pawn  -----------------------------------------------

% Een pion kan twee plaatsen naar voor gaan als hij nog op zijn rank staat. normaal kan hij eentje naar voor gaan.


piece_rule(pawn, From, To, Board) :-
    on_board(From), on_board(To),
    color_of_co(From, Board, Color),
    pawn_direction(Color, Direction),
    can_pawn_move(Color, Direction, From, To, Board).

% Basic forward move (1 step)
can_pawn_move(_, Dir, (R1, C), (R2, C), Board) :-
    R2 is R1 + Dir,
    co_empty((R2, C), Board).

% Initial two-step move
can_pawn_move(Color, Dir, (R1, C), (R3, C), Board) :-
    pawn_start_row(Color, R1),
    R2 is R1 + Dir,                 % Intermediate square
    R3 is R1 + Dir * 2,             % Final square
    co_empty((R2, C), Board),
    co_empty((R3, C), Board).

% Capture move
can_pawn_move(Color, Dir, (R1, C1), (R2, C2), Board) :-
    R2 is R1 + Dir,                 % Pawn must go one square in its direction.
    abs(C2 - C1) =:= 1,             % Taking is done one square diagonally
    opponent(Color, OpponentColor),
    get((R2, C2), Board, Piece),
    color(Piece, OpponentColor).    % Piece on attacking square must be of opposite color.
