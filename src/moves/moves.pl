
% ---------------------------------------------------------------------------------------------------------
%                                                   Moves 
% ---------------------------------------------------------------------------------------------------------
% In this module, the possible move rules of every piece are defined. This is not with respect to a board,
%           where the set of possible moves is reduced if other pieces are in the way. 
% ---------------------------------------------------------------------------------------------------------

% ------------------------------------------------ General ------------------------------------------------

hyper_move(move(From, To), Color, Board, BoardInfo, NewBoard, BoardInfo) :-
    move(From, To, Color, Board, NewBoard).

hyper_move(castle(Type), Color, Board, BoardInfo, NewBoard, _) :-
    is_legal_move(Type, Color, Board, BoardInfo),
    castle(Type, Color, Board, BoardInfo, NewBoard, _).


move(From, To, Color, Board, NewBoard) :-
    is_legal_move(From, To, Color, Board),
    move(From, To, Board, NewBoard).


is_legal_move(From, To, Color, Board) :-
    basic_move_validity(From, To, Color, Board),
    move(From, To, Board, NewBoard),    % Get the board on which the desired move was made.
    color_of_co(From, Board, Color),    % Only pieces of the moving color can move.
    \+ king_in_check(NewBoard, Color).  % The own king cannot be in check in this position.
    

basic_move_validity(From, To, Color, Board) :-
    get(From, Board, FromPiece),
    color(FromPiece, Color),
    get(To, Board, ToSquare),
    name(FromPiece, FromName),               
    could_capture(FromPiece, ToSquare),     % Attacking square must be empty or of opposite color.
    basic_piece_movement(FromName, From, To, Board).    %


king_in_check(Board, Color) :- 
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    basic_move_validity(_, KingCo, OpponentColor, Board).

is_checkmate(Board, Color) :- 
    king_in_check(Board, Color),
    \+ is_legal_move(_, _, Color, Board).


is_stalemate(Board, ToMoveColor) :-
    \+ king_in_check(Board, ToMoveColor),
    \+ is_legal_move(_, _, ToMoveColor, Board).
