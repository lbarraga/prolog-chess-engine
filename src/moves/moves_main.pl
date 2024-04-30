:- module(moves_main, [legal_move/3, king_in_check/1, is_checkmate/1, is_stalemate/1]).
:- use_module('../pieces.pl').
:- use_module('../board.pl').
:- use_module(piece_rules).
:- use_module(castling).

% legal_move(?Move, +State, ?NewState)
% Move is a legal move in the given State, resulting in NewState.
% Move could be:
%   - move(From, To)
%   - castle(Type)
%   - promotion(From, To, PromotingPieceName)
%
% This is the main predicate for checking if a move is legal.
legal_move(castle(Type), State, NewState) :- castle(Type, State, NewState).

% Promotion move
legal_move(promotion(From, To, PromotingPieceName), state(Board, Info, Color), state(NewBoard, NewInfo, Opponent)) :-
    legal_move_promotion(move(From, To), state(Board, Info, Color), state(TempBoard, NewInfo, Opponent)),
    get(From, Board, Piece),
    name(Piece, pawn),                              % The piece is a pawn.
    color(Piece, Color),                            % The piece is of the correct color.
    last_rank(Color, To),                           % The pawn is moving to the last rank.
    name(PromotingPiece, PromotingPieceName),       % The promoting piece is a valid piece.
    is_promotable(PromotingPieceName),              % The promoting piece is promotable (rook, knight, bishop, queen).
    color(PromotingPiece, Color),                   % Get the correct atom for the promoting piece.
    place(To, PromotingPiece, TempBoard, NewBoard). % Place the promoting piece on the board.

% Normal move
legal_move(move(From, To), State, NewState) :-
    basic_move_unsafe(move(From, To), State, NewState), % Check if move could be done, disregarding check.
    NewState = state(NewBoard, Info, Opponent),
    opponent(Opponent, Color),                          % Get the opponent color.
    \+ king_in_check(state(NewBoard, Info, Color)),     % Check if the king is not in check after the move.

    % Check if it is not a pawn moving to the last last_rank (if it is, it should be a promotion move.
    State = state(Board, _, _),
    get(From, Board, Piece),
    \+ (name(Piece, pawn), last_rank(Color, To)).

% legal_move_promotion(+Move, +State, -NewState)
% Same as legal_move, but only for promotion moves (no check is done for pawn on last rank).
legal_move_promotion(move(From, To), State, NewState) :-
    basic_move_unsafe(move(From, To), State, NewState),
    NewState = state(NewBoard, Info, Opponent),
    opponent(Opponent, Color),
    \+ king_in_check(state(NewBoard, Info, Color)).

% is_promotable(+PieceName)
% Check if a this piece can be promoted to.
is_promotable(queen).
is_promotable(rook).
is_promotable(bishop).
is_promotable(knight).

% last_rank(+Color, +Position)
% Check if the position is the last rank for the given color.
last_rank(white, (0, _)).
last_rank(black, (7, _)).

% basic_move_unsafe(+Move, +State, -NewState)
% Checks if a move follows the piece movement rules, disregarding check.
basic_move_unsafe(Move, State, NewState) :-
    opposing_color_or_empty(Move, State),                   % Destination square is empty or contains an opponent piece.
    get_name(Move, State, Name),                            % Get the name of the piece.
    basic_piece_move_unsafe(Name, Move, State, NewState).   % Check if the piece can move to the destination square. (piece rule)

% get_name(+Move, +State, -Name)
% Get the name of the piece that is moving.
get_name(move(From, _), state(Board, _, _), Name) :-
    get(From, Board, Piece),
    name(Piece, Name).

% opposing_color_or_empty(+Move, +State)
% Check if the destination square is empty or contains an opponent piece.
opposing_color_or_empty(move(From, To), state(Board, _, Color)) :-
    get(From, Board, FromPiece),
    color(FromPiece, Color),
    get(To, Board, ToSquare),
    could_capture(FromPiece, ToSquare). % The destination square is empty or contains an opponent piece.

% king_in_check(+State)
% Check if the king is in check in the given state.
king_in_check(state(Board, _, Color)) :-
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    basic_move_unsafe(move(_, KingCo), state(Board, _, OpponentColor), _). % Check if the king can be captured, disregarding pins or checks.

% is_stalemate(+State)
% Check if the game is in stalemate.
is_stalemate(State) :-
    \+ king_in_check(State),    % King is not in check
    \+ legal_move(_, State, _). % No legal moves

% is_checkmate(+State)
% Check if the game is in checkmate.
is_checkmate(State) :-
    king_in_check(State),       % King is in check
    \+ legal_move(_, State, _), !. % No legal moves

% is_checkmate(+State)
% Additional check for checkmate in king of the hill. The opponent is checkmated when the king is on a central square.
is_checkmate(State) :-
    rules(koth_rules),
    state(Board, _, Color) = State,
    opponent(Color, OpponentColor),
    king(OpponentColor, OpponentKing),
    get(OpponentKingCo, Board, OpponentKing),
    central_square(OpponentKingCo).     % King is on a central square

% central_square(+Position)
% Check if the position is a central square.
central_square((3,3)).
central_square((3,4)).
central_square((4,3)).
central_square((4,4)).