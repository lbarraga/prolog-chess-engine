legal_move(castle(Type), GameState, NewGameState) :-
    castle(Type, GameState, NewGameState).

legal_move(promotion(From, To, Promotion), GameState, NewGameState) :-
    legal_move(From, To, GameState, NewGameState).

legal_move(move(From, To), GameState, NewGameState) :-
    basic_move_unsafe(From, To, GameState, NewGameState),
    \+ in_check(NewGameState).


basic_move_unsafe(From, To, GameState, NewGameState) :-
    opposing_color_or_empty(From, To, GameState),
    basic_piece_move_unsafe(From, To, GameState, NewGameState).

rook_move_unsafe(From, To, GameState, NewGameState) :-
    can_move_unsafe(rook, From, To, Board),
