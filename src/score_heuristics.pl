% Very low score indicating the player has lost.
worst_score(white, -1000).
worst_score(black, 1000).

% Checkmate gives the worst possible score.
score_heuristic(Board, Color, Score) :-     
    is_checkmate(Board, Color),
    worst_score(Color, Score), 
    !.

% A Stalemate will gave an immediate score of 0.
score_heuristic(Board, ToMoveColor, 0) :- 
    is_stalemate(Board, ToMoveColor), 
    !.

% The total score of a board position is the score of white subtracted with the score of black.
score_heuristic(Board, _, Score) :- 
    score_heuristic_color(Board, white, WhiteScore),
    score_heuristic_color(Board, black, BlackScore),
    Score is WhiteScore - BlackScore.


% Give the score of a color by looking at, pieces and (center) square control.
score_heuristic_color(Board, Color, Score) :- 
    piece_score(Color, Board, PieceScore),
    amount_of_center_controls(Color, Board, CenterControl),
    amount_of_controls(Color, Board, Controls),

    Score is (
        PieceScore + 
        CenterControl * 0.1 + 
        Controls * 0.02
    ).

% ========== 1. Piece Score ==========

piece_score(Color, Board, SumOfValues) :- 
    findall(Value, value_of_piece_on_board(Color, Board, Value), Values),
    sum_list(Values, SumOfValues).

% ========== 2. Square Control ==========

amount_of_controls(Color, Board, Amount) :- 
    findall(From, controls(From, _, Color, Board), Controllers),
    length(Controllers, Amount).

controls(From, To, Color, Board) :- 
    get(From, Board, Piece),
    not(name(Piece, pawn)),
    uber_can_move(From, To, Color, Board).


% ========== 3. Center Square Control ==========

amount_of_center_controls(Color, Board, Amount) :- 
    findall(From, controls_center(Color, Board, From), CenterControllingPieces),
    length(CenterControllingPieces, Amount).

controls_center(Color, Board, From) :- 
    is_center(CenterSquare),
    uber_can_move(From, CenterSquare, Color, Board).

is_center((3, 3)).
is_center((3, 4)).
is_center((4, 3)).
is_center((4, 4)).