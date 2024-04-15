:- module(alpha_beta, [best_move/3]).
:- use_module('moves/moves_main.pl', [legal_move/3]).
:- use_module(score_heuristics).

% Find all legal moves for a given state
children(State, Children) :-
    bagof(
        bm(ChildState, Move, Score),
        (legal_move(Move, State, ChildState),
        score_heuristic(ChildState, Score)),
        Children
    ).

% Sort the list of children based on their score
sort_children(Children, SortedChildren) :- predsort(compare_child_score, Children, SortedChildren).

% Comparison predicate for sorting children based on score
compare_child_score(Order, bm(state(_, _, white), _, Score1), bm(_, _, Score2)) :- compare(Order, Score1, Score2).
compare_child_score(Order, bm(state(_, _, black), _, Score1), bm(_, _, Score2)) :- compare(Order, Score2, Score1).

% ---------------------------------

best_move(State, Depth, Move) :- alpha_beta(State, Depth, -1000, +1000, Move).

% If the depth is 0, return the heuristic value of the state
alpha_beta(State, 0, _, _, vm(Value, no_move)) :-
    score_heuristic(State, Value), !.

% If there are children, recursively call alpha_beta on them
alpha_beta(State, Depth, Alpha, Beta, ValueMove) :-
    NewDepth is Depth - 1,
    worst_score(State, WorstScore),
    children(State, Children),
    sort_children(Children, SortedChildren),
    do_children(SortedChildren, NewDepth, Alpha, Beta, vm(WorstScore, no_move), ValueMove), !.

% If there are no children, return the heuristic value of the state
alpha_beta(State, _, _, _, vm(Score, no_move)) :- score_heuristic(State, Score).

do_children([], _, _, _, Acc, Acc).
do_children([bm(ChildState, Move, _) | Rest], Depth, Alpha, Beta, Acc, ValueMove) :-
    comp_mode(ChildState, CompMode, Color),
    alpha_beta(ChildState, Depth, Alpha, Beta, vm(Score, _)),

    % Compare the new score the the current best score
    compare_mode(Acc, vm(Score, Move), NewAcc, CompMode),
    update_alpha_beta(Color, Alpha, Beta, Score, NewAlpha, NewBeta),

    NewAcc = vm(BestScore, _),
    (
        should_prune(Color, BestScore, NewAlpha, NewBeta)
        -> ValueMove = NewAcc
        ; do_children(Rest, Depth, NewAlpha, NewBeta, NewAcc, ValueMove)
    ).

% White is the maximizing player, black is the minimizing player
update_alpha_beta(white, Alpha, Beta, Score, NewAlpha, Beta) :- NewAlpha is max(Alpha, Score).
update_alpha_beta(black, Alpha, Beta, Score, Alpha, NewBeta) :- NewBeta is min(Beta, Score).

% Predicate to get the comparison mode based on the player
comp_mode(state(_, _, white), min, black).
comp_mode(state(_, _, black), max, white).

% Adjusted compare_mode predicate to handle vm structures.
compare_mode(vm(V1, M1), vm(V2, _),  vm(V1, M1), min) :- V1 < V2.
compare_mode(vm(V1, _),  vm(V2, M2), vm(V2, M2), min) :- V1 >= V2.
compare_mode(vm(V1, M1), vm(V2, _),  vm(V1, M1), max) :- V1 > V2.
compare_mode(vm(V1, _),  vm(V2, M2), vm(V2, M2), max) :- V1 =< V2.


% white should prune when the best score is greater than beta while black should prune when the best score is less than alpha
should_prune(white, BestScore, _, Beta) :- BestScore > Beta.
should_prune(black, BestScore, Alpha, _) :- BestScore < Alpha.