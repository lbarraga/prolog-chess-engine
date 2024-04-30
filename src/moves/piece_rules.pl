% pieces.pl, board.pl
:- module(piece_rules, [basic_piece_move_unsafe/4]).
:- use_module('../pieces.pl').
:- use_module('../board.pl').
:- use_module(update_info).

% basic_piece_move_unsafe(+Name, +Move, +State, -NewState)
% Move a piece on the board and update the game state.
% This predicate checks for piece rules and valid destination squares, but does not check for check.
basic_piece_move_unsafe(Name, Move, state(Board, Info, ToMove), state(NewBoard, NewInfo, Opponent)) :-
    opponent(ToMove, Opponent),                         % Get opponent color
    piece_rule(Name, Move, state(Board, Info, ToMove)), % Check if move is valid for piece
    move_unsafe(Move, Board, TempBoard),                % Move piece on board
    update_info(Name, Move, ToMove, Info, NewInfo),
    check_en_passant(Move, Info, TempBoard, NewBoard).

% check_en_passant(+Move, +Info, +Board, -NewBoard)
% If the move is an en passant capture, remove the captured pawn
check_en_passant(move(_, To), info(_, To), Board, NewBoard) :-
    get(To, Board, Piece),
    name(Piece, pawn),
    one_after_en_passant_square(To, After),
    remove(After, Board, NewBoard), !.

% Do nothing if not en passant
check_en_passant(_, _, Board, Board).

% en_passant_square(+Square)
% Square is a square where an en passant capture can be made
en_passant_square((2, _)).
en_passant_square((5, _)).

% one_after_en_passant_square(+Square, -After)
% After is the square after Square
one_after_en_passant_square((5, C), (4, C)).
one_after_en_passant_square((2, C), (3, C)).

% ------------------------------------------------- Rook -----------------------------------------------
% piece_rule(+Piece, +Move, +State)
% Checks if a piece can move from From to To in the given state.
% This predicate checks that pieces can only capture opponents pieces and that they can only move to empty squares.
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

% Top level pawn rule.
piece_rule(pawn, move(From, To), state(Board, info(_, EP), _)) :-
    on_board(From), on_board(To),
    color_of_co(From, Board, Color),    % Get color of pawn
    pawn_direction(Color, Direction),   % Get direction of pawn
    pawn_rule(Color, Direction, move(From, To), Board, EP).

% Basic forward move (1 step)
pawn_rule(_, Dir, move(From, OneForward), Board, _) :-
    one_forward(From, OneForward, Dir),
    co_empty(OneForward, Board).

% Double forward move (2 steps)
pawn_rule(Color, Dir, move(From, TwoForward), Board, _) :-
    pawn_start_row(Color, From),        % Pawn must be on starting row
    one_forward(From, OneForward, Dir), % Get one step forward
    two_forward(From, TwoForward, Dir), % Get two steps forward
    co_empty(OneForward, Board),        % One step forward must be empty
    co_empty(TwoForward, Board).        % Two steps forward must be empty

% Capture move
pawn_rule(Color, Dir, move(From, To), Board, _) :-
    one_diagonal(From, To, Dir),    % To must be one diagonal from From
    opponent(Color, OpponentColor),
    get(To, Board, Piece),
    color(Piece, OpponentColor).    % Piece on attacking square must be of opposite color.

% En Passant
pawn_rule(_, Dir, move(From, To), _, To) :- one_diagonal(From, To, Dir).


% -------------------------------------------------  Helper  -----------------------------------------------

l_shape(Co1, Co2) :- delta(Co1, Co2, (1, 2)).
l_shape(Co1, Co2) :- delta(Co1, Co2, (2, 1)).

delta((R1, C1), (R2, C2), (DeltaR, DeltaC)) :-
    DeltaR is abs(R2 - R1),
    DeltaC is abs(C2 - C1).

one_forward((R1, C), (R2, C), Dir) :- R2 is R1 + Dir.
two_forward((R1, C), (R3, C), Dir) :- R3 is R1 + Dir * 2.
one_diagonal((R1, C1), (R2, C2), Dir) :- R2 is R1 + Dir, abs(C2 - C1) =:= 1.