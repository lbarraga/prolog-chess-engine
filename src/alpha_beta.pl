:- module(alpha_beta, [best_move/3]).
:- use_module('moves/moves_main.pl', [legal_move/3, is_checkmate/1]).
:- use_module(score_heuristics).

compare_children(Order, bm(State1, _), bm(State2, _)) :-
    score_heuristic(State1, Score1),
    score_heuristic(State2, Score2),
    comp_mode(State1, Mode),
    (Mode = max -> (Score1 > Score2 -> Order = > ; Order = <) ; (Score1 < Score2 -> Order = > ; Order = <)).


children(State, Children) :-
    bagof(bm(NewState, M), legal_move(M, State, NewState), Children).

best_move(State, Depth, Move) :- alpha_beta(State, Depth, -1000, 1000, Move).

% If the depth is 0, return the heuristic value of the state
alpha_beta(State, 0, _, _, vm(Value, no_move)) :-
    score_heuristic(State, Value), !.

alpha_beta(State, Depth, Alpha, Beta, vm(Value, Move)) :-
    NewDepth is Depth - 1,
    \+ is_checkmate(State),
    children(State, Children),
    comp_mode(State, Mode),
    worst_score(State, WorstScore),
    choose_child(Children, NewDepth, Alpha, Beta, Mode, vm(WorstScore, no_move), vm(Value, Move)), !.

% If there are no children, return the heuristic value of the state
alpha_beta(State, _, _, _, vm(Score, no_move)) :- score_heuristic(State, Score).

% function alphabeta(node, depth, α, β, maximizingPlayer) is
%     if maximizingPlayer then
%         value := −∞
%         for each child of node do
%             value := max(value, alphabeta(child, depth − 1, α, β, FALSE))
%             if value > β then
%                 break (* β cutoff *)
%             α := max(α, value)
%         return value
%     else
%         value := +∞
%         for each child of node do
%             value := min(value, alphabeta(child, depth − 1, α, β, TRUE))
%             if value < α then
%                 break (* α cutoff *)
%             β := min(β, value)
%         return value

choose_child([], _, _, _, _, Acc, Acc).
choose_child([bm(ChildState, Move) | Children], Depth, Alpha, Beta, Mode, Acc, BestMove) :-
    % Get the value of the child
    alpha_beta(ChildState, Depth, Alpha, Beta, vm(Value, _)),

    % Update the best value and move
    compare_moves(Mode, vm(Value, Move), Acc, NewAcc),

    % Update alpha or beta based on the mode
    update_alpha_beta(Mode, Alpha, Beta, Value, NewAlpha, NewBeta),

    % Check if we should prune
    (
        should_prune(Mode, NewAlpha, NewBeta, NewAcc)
        -> BestMove = NewAcc
        ; choose_child(Children, Depth, NewAlpha, NewBeta, Mode, NewAcc, BestMove)
    ).


should_prune(max, _, Beta, vm(Value, _)) :- Value > Beta.
should_prune(min, Alpha, _, vm(Value, _)) :- Value < Alpha.

update_alpha_beta(max, Alpha, Beta, Score, NewAlpha, Beta) :- NewAlpha is max(Alpha, Score).
update_alpha_beta(min, Alpha, Beta, Score, Alpha, NewBeta) :- NewBeta is min(Beta, Score).

compare_moves(max, vm(V1, M), vm(V2, _), vm(V1, M)) :- V1 >= V2, !.
compare_moves(max, vm(V1, _), vm(V2, M), vm(V2, M)) :- V1 <  V2, !.
compare_moves(min, vm(V1, M), vm(V2, _), vm(V1, M)) :- V1 <  V2, !.
compare_moves(min, vm(V1, _), vm(V2, M), vm(V2, M)) :- V1 >= V2, !.

comp_mode(state(_, _, white), max).
comp_mode(state(_, _, black), min).