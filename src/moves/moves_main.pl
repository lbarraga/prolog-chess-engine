:- module(moves_main, [legal_move/3, king_in_check/1, is_checkmate/1, is_stalemate/1]).
:- use_module('../pieces.pl').
:- use_module('../board.pl').
:- use_module(piece_rules).
:- use_module(castling).

legal_move(castle(Type), State, NewState) :-
    castle(Type, State, NewState).

legal_move(promotion(From, To, PromotingPieceName), state(Board, Info, Color), state(NewBoard, NewInfo, Opponent)) :-
    legal_move_promotion(move(From, To), state(Board, Info, Color), state(TempBoard, NewInfo, Opponent)),
    % Check if the piece moved is a pawn
    get(From, Board, Piece),
    name(Piece, pawn),
    color(Piece, Color),
    last_rank(Color, To),
    name(PromotingPiece, PromotingPieceName),
    is_promotable(PromotingPieceName),
    color(PromotingPiece, Color),
    place(To, PromotingPiece, TempBoard, NewBoard).


legal_move(move(From, To), State, NewState) :-
    basic_move_unsafe(move(From, To), State, NewState),
    NewState = state(NewBoard, Info, Opponent),
    opponent(Opponent, Color),
    \+ king_in_check(state(NewBoard, Info, Color)),
    % Check if it is not a pawn moving to the last last_rank
    State = state(Board, _, _),
    get(From, Board, Piece),
    \+ (name(Piece, pawn), last_rank(Color, To)).

legal_move_promotion(move(From, To), State, NewState) :-
    basic_move_unsafe(move(From, To), State, NewState),
    NewState = state(NewBoard, Info, Opponent),
    opponent(Opponent, Color),
    \+ king_in_check(state(NewBoard, Info, Color)).

is_promotable(queen).
is_promotable(rook).
is_promotable(bishop).
is_promotable(knight).

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


king_in_check(state(Board, _, Color)) :-
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    basic_move_unsafe(move(_, KingCo), state(Board, _, OpponentColor), _).

is_stalemate(State) :-
    \+ king_in_check(State),    % King is not in check
    \+ legal_move(_, State, _). % No legal moves

is_checkmate(State) :-
    king_in_check(State),       % King is in check
    \+ legal_move(_, State, _). % No legal moves