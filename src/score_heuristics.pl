:- module(score_heuristics, [score_heuristic/2, worst_score/2]).
:- use_module('moves/moves_main.pl', [is_checkmate/1, is_stalemate/1, legal_move/3]).
:- use_module('pieces.pl').

% Very low score indicating the player has lost.
worst_score(state(_, _, white), -1000).
worst_score(state(_, _, black), +1000).

% Checkmate gives the worst possible score.
score_heuristic(State, WorstScore) :- is_checkmate(State), worst_score(State, WorstScore), !.

% A Stalemate will gave an immediate score of 0.
score_heuristic(State, 0) :- is_stalemate(State), !.

% The total score of a board position is the score of white subtracted with the score of black.
score_heuristic(state(Board, Info, _), Score) :-
    score_heuristic_color(state(Board, Info, white), WhiteScore),
    score_heuristic_color(state(Board, Info, black), BlackScore),
    Score is WhiteScore - BlackScore.

% Give the score of a color by looking at pieces and (center) square control.
score_heuristic_color(State, Score) :-
    piece_score(State, PieceScore),
    amount_of_center_controls(State, CenterControl),
    amount_of_controls(State, Controls),

    Score is (
        PieceScore + 
        CenterControl * 0.1 + 
        Controls * 0.02
    ).

% ========== 1. Piece Score ==========

piece_score(state(Board, _, Color), SumOfValues) :-
    findall(Value, value_of_piece_on_board(Color, Board, Value), Values),
    sum_list(Values, SumOfValues).

% ========== 2. Square Control ==========

amount_of_controls(State, Amount) :-
    findall(Move, controls(Move, State), Controllers),
    length(Controllers, Amount).

controls(move(From, To), state(Board, Info, Color)) :-
    get(From, Board, Piece),
    not(name(Piece, pawn)),
    legal_move(move(From, To), state(Board, Info, Color), _).

% ========== 3. Center Square Control ==========

amount_of_center_controls(State, Amount) :-
    findall(From, controls_center(State, From), CenterControllingPieces),
    length(CenterControllingPieces, Amount).

controls_center(State, From) :-
    is_center(CenterSquare),
    legal_move(move(From, CenterSquare), State, _).

is_center((3, 3)).
is_center((3, 4)).
is_center((4, 3)).
is_center((4, 4)).