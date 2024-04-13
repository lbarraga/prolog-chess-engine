basic_piece_move_unsafe(Name, Move, state(Board, Info, ToMove), state(NewBoard, NewInfo, Opponent)) :-
    opponent(ToMove, Opponent),
    piece_rule(Name, Move, state(Board, Info, ToMove)),
    move_unsafe(Move, Board, TempBoard),
    update_info(Name, Move, ToMove, Info, NewInfo),
    check_en_passant(Move, Info, TempBoard, NewBoard).


% ------------------------------------------------- Rook -----------------------------------------------
piece_rule(rook, move(From, To), state(Board, _, _)) :- in_sight(row, From, To, Board).
piece_rule(rook, move(From, To), state(Board, _, _)) :- in_sight(column, From, To, Board).

% ------------------------------------------------- Bishop -----------------------------------------------
piece_rule(bishop, move(From, To), state(Board, _, _)) :- in_sight(diagonal, From, To, Board).
piece_rule(bishop, move(From, To), state(Board, _, _)) :- in_sight(anti_diagonal, From, To, Board).

% -------------------------------------------------  Queen  -----------------------------------------------
piece_rule(queen, Move, State) :- piece_rule(bishop, Move, State).
piece_rule(queen, Move, State) :- piece_rule(rook, Move, State).

% -------------------------------------------------  Knight  -----------------------------------------------
piece_rule(knight, move(Co1, Co2), _) :-
    on_board(Co1), on_board(Co2),
    l_shape(Co1, Co2).

% -------------------------------------------------  King  -----------------------------------------------

piece_rule(king, move((R1, C1), (R2, C2)), _) :-
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =< 1,
    abs(C2 - C1) =< 1.

% -------------------------------------------------  Pawn  -----------------------------------------------

% Een pion kan twee plaatsen naar voor gaan als hij nog op zijn rank staat. normaal kan hij eentje naar voor gaan.


piece_rule(pawn, move(From, To), state(Board, info(_, EP), _)) :-
    on_board(From), on_board(To),
    color_of_co(From, Board, Color),    % Get color of pawn
    pawn_direction(Color, Direction),   % Get direction of pawn
    can_pawn_move(Color, Direction, move(From, To), Board, EP).

% Basic forward move (1 step)
can_pawn_move(_, Dir, move(From, OneForward), Board, _) :-
    one_forward(From, OneForward, Dir),
    co_empty(OneForward, Board).

% Double forward move (2 steps)
can_pawn_move(Color, Dir, move(From, TwoForward), Board, _) :-
    pawn_start_row(Color, From),        % Pawn must be on starting row
    one_forward(From, OneForward, Dir), % Get one step forward
    two_forward(From, TwoForward, Dir), % Get two steps forward
    co_empty(OneForward, Board),        % One step forward must be empty
    co_empty(TwoForward, Board).        % Two steps forward must be empty

% Capture move
can_pawn_move(Color, Dir, move(From, To), Board, _) :-
    one_diagonal(From, To, Dir),    % To must be one diagonal from From
    opponent(Color, OpponentColor),
    get(To, Board, Piece),
    color(Piece, OpponentColor).    % Piece on attacking square must be of opposite color.

% En Passant
can_pawn_move(_, Dir, move(From, To), _, To) :- one_diagonal(From, To, Dir).


% -------------------------------------------------  Helper  -----------------------------------------------

l_shape(Co1, Co2) :- delta(Co1, Co2, (1, 2)).
l_shape(Co1, Co2) :- delta(Co1, Co2, (2, 1)).

delta((R1, C1), (R2, C2), (DeltaR, DeltaC)) :-
    DeltaR is abs(R2 - R1),
    DeltaC is abs(C2 - C1).

one_forward((R1, C), (R2, C), Dir) :- R2 is R1 + Dir.
two_forward((R1, C), (R3, C), Dir) :- R3 is R1 + Dir * 2.
one_diagonal((R1, C1), (R2, C2), Dir) :- R2 is R1 + Dir, abs(C2 - C1) =:= 1.