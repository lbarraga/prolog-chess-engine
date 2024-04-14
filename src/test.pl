playing_depth(2).

play :-
    init_state(State),
    play(State).

play(State) :-
    playing_depth(Depth),

    % White's turn
    best_move(State, Depth, vm(_, WhiteMove))
    write("Move found for White: "), writeln(WhiteMove),
    legal_move(WhiteMove, State, WhiteState),

    % Black's turn
    best_move(WhiteState, Depth, vm(_, BlackMove)),
    write("Move found for Black: "), writeln(BlackMove),
    legal_move(BlackMove, WhiteState, NextState),

    play(NextState).