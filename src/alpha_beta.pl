:- module(alpha_beta, [best_move/3]).
:- use_module('moves/moves_main.pl', [legal_move/3, is_checkmate/1]).
:- use_module(score_heuristics).

% best_move(+State, +Depth, -Move)
% Returns the best move for the given state and depth
best_move(State, Depth, Move) :- alpha_beta(State, Depth, -1000, 1000, Move).

% children(+State, -Children)
% Returns a list of all the children of the given state, along with the move that led to them
children(State, Children) :- bagof(state_move(NewState, M), legal_move(M, State, NewState), Children).

% alpha_beta(+State, +Depth, +Alpha, +Beta, -ValueMove)
% Returns the best value and move for the given state, depth, alpha and beta
% Return the heuristic value of the state if the depth is 0.
alpha_beta(State, 0, _, _, value_move(Value, no_move)) :-
    score_heuristic(State, Value), !.

% The current state has children and is not a checkmate
alpha_beta(State, Depth, Alpha, Beta, value_move(Value, Move)) :-
    NewDepth is Depth - 1,
    \+ is_checkmate(State),
    children(State, Children),      % Get all the children of the current state
    comp_mode(State, Mode),         % Get the comparison mode of the current state (max or min)
    worst_score(State, WorstScore), % Get the worst possible score for the current state
    % Choose the best child based on the comparison mode
    choose_child(Children, NewDepth, Alpha, Beta, Mode, value_move(WorstScore, no_move), value_move(Value, Move)), !.

% If there are no children or the current state is checkmate, return the heuristic value of the state
alpha_beta(State, _, _, _, value_move(Score, no_move)) :- score_heuristic(State, Score).

% choose_child(+Children, +Depth, +Alpha, +Beta, +Mode, +Acc, -BestMove)
% Chooses the best child based on the comparison mode
choose_child([], _, _, _, _, Acc, Acc).
choose_child([state_move(ChildState, Move) | Children], Depth, Alpha, Beta, Mode, Acc, BestMove) :-
    % Get the value of the child
    alpha_beta(ChildState, Depth, Alpha, Beta, value_move(Value, _)),

    % Update the best value and move
    compare_moves(Mode, value_move(Value, Move), Acc, NewAcc),

    % Update alpha or beta based on the mode
    update_alpha_beta(Mode, Alpha, Beta, Value, NewAlpha, NewBeta),

    % Check if we should prune
    (
        should_prune(Mode, NewAlpha, NewBeta, NewAcc)
        -> BestMove = NewAcc
        ; choose_child(Children, Depth, NewAlpha, NewBeta, Mode, NewAcc, BestMove)
    ).


% should_prune(+Mode, +Alpha, +Beta, +ValueMove)
% True if we should prune the current branch
should_prune(max, _, Beta, value_move(Value, _)) :- Value > Beta.
should_prune(min, Alpha, _, value_move(Value, _)) :- Value < Alpha.

% update_alpha_beta(+Mode, +Alpha, +Beta, +Score, -NewAlpha, -NewBeta)
% Updates the alpha or beta value based on the comparison mode
update_alpha_beta(max, Alpha, Beta, Score, NewAlpha, Beta) :- NewAlpha is max(Alpha, Score).
update_alpha_beta(min, Alpha, Beta, Score, Alpha, NewBeta) :- NewBeta is min(Beta, Score).

% compare_moves(+Mode, +ValueMove1, +ValueMove2, -BestMove)
% Compares two value moves and choose the 'best' based on the comparison mode
compare_moves(max, value_move(V1, M), value_move(V2, _), value_move(V1, M)) :- V1 >= V2, !.
compare_moves(max, value_move(V1, _), value_move(V2, M), value_move(V2, M)) :- V1 <  V2, !.
compare_moves(min, value_move(V1, M), value_move(V2, _), value_move(V1, M)) :- V1 <  V2, !.
compare_moves(min, value_move(V1, _), value_move(V2, M), value_move(V2, M)) :- V1 >= V2, !.

% worst_score(+State, -WorstScore)
% white is the maximizing player and black is the minimizing player.
comp_mode(state(_, _, white), max).
comp_mode(state(_, _, black), min).