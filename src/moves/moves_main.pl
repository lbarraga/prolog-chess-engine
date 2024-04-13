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



king_in_check(state(Board, _, Color)) :-
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    basic_move_unsafe(move(_, KingCo), state(Board, _, OpponentColor), _).