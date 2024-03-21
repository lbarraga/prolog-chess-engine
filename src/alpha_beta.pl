child_board(ToMoveColor, Board, move(From, To), ChildBoard) :- uber_move(From, To, ToMoveColor, Board, ChildBoard).

children(ToMoveColor, Board, Children) :-
    findall(
        bm(ChildBoard, move(From, To)), 
        child_board(ToMoveColor, Board, move(From, To), ChildBoard), 
        Children
    ).


% Adjusted compare_mode predicate to handle vm structures.
compare_mode(vm(V1, M1), vm(V2, _),  vm(V1, M1), min) :- V1 < V2.
compare_mode(vm(V1, _),  vm(V2, M2), vm(V2, M2), min) :- V1 >= V2.
compare_mode(vm(V1, M1), vm(V2, _),  vm(V1, M1), max) :- V1 > V2.
compare_mode(vm(V1, _),  vm(V2, M2), vm(V2, M2), max) :- V1 =< V2.

% ---------------------------------

best_move(Board, ToMoveColor, Depth, Move) :- alpha_beta(Board, Depth, ToMoveColor, -1000, +1000, Move).

alpha_beta(Board, 0, ToMoveColor, _, _, vm(Value, no_move)) :- score_heuristic(Board, ToMoveColor, Value), !.


alpha_beta(Board, _, Color, _, _, vm(Score, no_move)) :- 
    children(Color, Board, []),
    score_heuristic(Board, Color, Score), !.


alpha_beta(Board, Depth, Color, Alpha, Beta, ValueMove) :- 
    NewDepth is Depth - 1,
    worst_score(Color, WorstScore),
    children(Color, Board, Children),
    do_children(Children, NewDepth, Color, Alpha, Beta, vm(WorstScore, no_move), ValueMove).



% Predicate to update Alpha/Beta based on the player
update_alpha_beta(white, Alpha, Beta, Score, max(Alpha, Score), Beta).
update_alpha_beta(black, Alpha, Beta, Score, Alpha, min(Beta, Score)).

% Predicate to get the comparison mode based on the player
comp_mode(white, max).
comp_mode(black, min).


should_prune(white, BestScore, _, Beta) :- BestScore > Beta.
should_prune(black, BestScore, Alpha, _) :- BestScore < Alpha.

do_children([], _, _, _, _, Acc, Acc).
do_children([bm(Child, Move) | Rest], Depth, Player, Alpha, Beta, Acc, ValueMove) :-
    opponent(Player, Opponent),
    comp_mode(Player, CompMode),

    alpha_beta(Child, Depth, Opponent, Alpha, Beta, vm(Score, _)),
    compare_mode(Acc, vm(Score, Move), NewAcc, CompMode),
    update_alpha_beta(Player, Alpha, Beta, Score, NewAlpha, NewBeta),

    NewAcc = vm(BestScore, _),
    (
        should_prune(Player, BestScore, NewAlpha, NewBeta)
        -> ValueMove = NewAcc
        ; do_children(Rest, Depth, Player, NewAlpha, NewBeta, NewAcc, ValueMove)
    ).