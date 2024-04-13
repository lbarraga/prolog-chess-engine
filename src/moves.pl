legal_move(castle(Type), State, NewState) :-
    castle(Type, State, NewState).

legal_move(promotion(From, To, PromotingPieceName), state(Board, Info, Color), state(NewBoard, NewInfo, Opponent)) :-
    legal_move(move(From, To), state(Board, Info, Color), state(TempBoard, NewInfo, Opponent)),
    % Check if the piece moved is a pawn
    get(From, Board, Piece),
    name(Piece, pawn),
    color(Piece, Color),
    last_rank(Color, To),
    name(PromotingPiece, PromotingPieceName),
    color(PromotingPiece, Color),
    place(To, PromotingPiece, TempBoard, NewBoard).


legal_move(move(From, To), State, NewState) :-
    basic_move_unsafe(move(From, To), State, NewState),
    NewState = state(NewBoard, Info, Opponent),
    opponent(Opponent, Color),
    \+ king_in_check(state(NewBoard, Info, Color)).

last_rank(white, (0, _)).
last_rank(black, (7, _)).

basic_move_unsafe(Move, State, NewState) :-
    opposing_color_or_empty(Move, State),
    get_name(Move, State, Name),
    basic_piece_move_unsafe(Name, Move, State, NewState).

get_name(move(From, _), state(Board, _, _), Name) :-
    get(From, Board, Piece),
    name(Piece, Name).

opposing_color_or_empty(move(From, To), state(Board, _, Color)) :-
    get(From, Board, FromPiece),
    color(FromPiece, Color),
    get(To, Board, ToSquare),
    could_capture(FromPiece, ToSquare).


basic_piece_move_unsafe(Name, Move, state(Board, Info, ToMove), state(NewBoard, NewInfo, Opponent)) :-
    opponent(ToMove, Opponent),
    piece_rule(Name, Move, state(Board, Info, ToMove)),
    move_unsafe(Move, Board, TempBoard),
    update_info(Name, Move, ToMove, Info, NewInfo),
    check_en_passant(Move, Info, TempBoard, NewBoard).

check_en_passant(move(_, (2, C)), info(_, (2, C)), Board, NewBoard) :-
    % Check if from is a pawn
    get((2, C), Board, Piece),
    name(Piece, pawn),
    remove((3, C), Board, NewBoard), !.

check_en_passant(move(_, (5, C)), info(_, (5, C)), Board, NewBoard) :-
    % Check if from is a pawn
    get((5, C), Board, Piece),
    name(Piece, pawn),
    remove((4, C), Board, NewBoard), !.

check_en_passant(_, _, Board, Board).


update_info(Name, Move, Color, info(Castling, EnPassant), info(NewCastling, NewEnPassant)) :-
    update_en_passant(Name, Move, EnPassant, NewEnPassant),
    update_castling_color(Color, ColorCastling, NewColorCastling, Castling, NewCastling),
    update_castling(Name, Move, ColorCastling, NewColorCastling).

disable_en_passant(info(Castling, _), info(Castling, no_ep)).

disable_short_castle((Long, _), (Long, no_short)).
disable_long_castle((_, Short), (no_long, Short)).

disable_castling(white, info(castling_info(_, Black), _), info(castling_info((no_long, no_short), Black), no_ep)).
disable_castling(black, info(castling_info(White, _), _), info(castling_info(White, (no_long, no_short)), no_ep)).

update_castling_color(white, Old, New, castling_info(Old, Black), castling_info(New, Black)).
update_castling_color(black, Old, New, castling_info(White, Old), castling_info(White, New)).

% En passant
update_en_passant(pawn, move((R1, C), (R3, C)), _, (R2, C)) :-
    abs(R3 - R1) =:= 2,
    R2 is (R1 + R3) // 2, !.

update_en_passant(_, _, _, no_ep).

% Castling
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




castle(Type, state(Board, Info, Color), state(NewBoard, NewInfo, Opponent)) :-
    opponent(Color, Opponent),
    disable_castling(Color, Info, NewInfo),
    can_castle(Type, state(Board, Info, Color)),
    castling_reqs(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos, _),
    move_unsafe(move(KingPos, TwoNextToKingPos), Board, TempBoard),
    move_unsafe(move(RookPos, OneNextToKingPos), TempBoard, NewBoard).


can_castle(Type, state(Board, info(Castling, _), Color)) :-
    \+ king_in_check(state(Board, _, Color)),
    castling_reqs(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos, Castling),
    co_empty(OneNextToKingPos, Board), co_empty(TwoNextToKingPos, Board),
    legal_move(move(RookPos, OneNextToKingPos), state(Board, _, Color), _),                         % Rook could move next to king.
    legal_move(move(KingPos, OneNextToKingPos), state(Board, _, Color), state(TempBoard, _, _)),    % King could move one square closer to rook.
    legal_move(move(OneNextToKingPos, TwoNextToKingPos), state(TempBoard, _, Color), _).            % King could move two squares closer to rook.



%                              King   King+1  King+2   Rook
castling_reqs(white, long,  (7, 4), (7, 3), (7, 2), (7, 0), castling_info((long, _), _)).
castling_reqs(white, short, (7, 4), (7, 5), (7, 6), (7, 7), castling_info((_, short), _)).
castling_reqs(black, long,  (0, 4), (0, 3), (0, 2), (0, 0), castling_info(_, (long, _))).
castling_reqs(black, short, (0, 4), (0, 5), (0, 6), (0, 7), castling_info(_, (_, short))).


king_in_check(state(Board, _, Color)) :-
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    basic_move_unsafe(move(_, KingCo), state(Board, _, OpponentColor), _).




piece_rule(rook, move(From, To), state(Board, _, _)) :- in_sight(row, From, To, Board).
piece_rule(rook, move(From, To), state(Board, _, _)) :- in_sight(column, From, To, Board).

% ------------------------------------------------- Bishop -----------------------------------------------

piece_rule(bishop, move(From, To), state(Board, _, _)) :- in_sight(diagonal, From, To, Board).
piece_rule(bishop, move(From, To), state(Board, _, _)) :- in_sight(anti_diagonal, From, To, Board).

% -------------------------------------------------  Queen  -----------------------------------------------

piece_rule(queen, Move, State) :- piece_rule(bishop, Move, State).
piece_rule(queen, Move, State) :- piece_rule(rook, Move, State).

% -------------------------------------------------  Knight  -----------------------------------------------

piece_rule(knight, move((R1, C1), (R2, C2)), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R1 - R2) =:= 1,
    abs(C2 - C1) =:= 2.

piece_rule(knight, move((R1, C1), (R2, C2)), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =:= 2,
    abs(C2 - C1) =:= 1.

% -------------------------------------------------  King  -----------------------------------------------

piece_rule(king, move((R1, C1), (R2, C2)), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =< 1,
    abs(C2 - C1) =< 1.

% -------------------------------------------------  Pawn  -----------------------------------------------

% Een pion kan twee plaatsen naar voor gaan als hij nog op zijn rank staat. normaal kan hij eentje naar voor gaan.


piece_rule(pawn, move(From, To), state(Board, info(_, EP), _)) :-
    on_board(From), on_board(To),
    color_of_co(From, Board, Color),
    pawn_direction(Color, Direction),
    can_pawn_move(Color, Direction, From, To, Board, EP).

% Basic forward move (1 step)
can_pawn_move(_, Dir, (R1, C), (R2, C), Board, _) :-
    R2 is R1 + Dir,
    co_empty((R2, C), Board).

% Initial two-step move
can_pawn_move(Color, Dir, (R1, C), (R3, C), Board, _) :-
    pawn_start_row(Color, R1),
    R2 is R1 + Dir,                 % Intermediate square
    R3 is R1 + Dir * 2,             % Final square
    co_empty((R2, C), Board),
    co_empty((R3, C), Board).

% Capture move
can_pawn_move(Color, Dir, (R1, C1), (R2, C2), Board, _) :-
    R2 is R1 + Dir,                 % Pawn must go one square in its direction.
    abs(C2 - C1) =:= 1,             % Taking is done one square diagonally
    opponent(Color, OpponentColor),
    get((R2, C2), Board, Piece),
    color(Piece, OpponentColor).    % Piece on attacking square must be of opposite color.

% En Passant
can_pawn_move(_, Dir, (R1, C1), (R2, C2), _, (R2, C2)) :-
    R2 is R1 + Dir,                 % Pawn must go one square in its direction.
    abs(C2 - C1) =:= 1.             % Taking is done one square diagonally
