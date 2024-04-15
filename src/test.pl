:- module(test, [play/0]).
:- use_module("moves/moves_main.pl").
:- use_module(prettyprint).

playing_depth(2).

play :-
    init_state(State),
    play(1, State).

play(N, State) :-
    playing_depth(Depth),

    % White's turn
    best_move(State, Depth, vm(_, WhiteMove)),
    % random_move(State, WhiteMove),
    write("Move found for White: "), writeln(WhiteMove),
    legal_move(WhiteMove, State, WhiteState),
    print_state(WhiteState),

    (   is_checkmate(WhiteState) ->  % Check if black is in checkmate after White's move
        writeln("Black is in checkmate! White wins.")
    ;   writeln("========================================"),
        % Black's turn
        % best_move(WhiteState, Depth, vm(_, BlackMove)),
        random_move(WhiteState, BlackMove),
        write("Move found for Black: "), writeln(BlackMove),
        legal_move(BlackMove, WhiteState, NextState),
        print_state(NextState),

        (   is_checkmate(NextState) ->  % Check if white is in checkmate after Black's move
            writeln("White is in checkmate! Black wins.")
        ;   N1 is N + 1,
            play(N1, NextState)  % Continue playing if neither side is in checkmate
        )
    ).


random_move(State, RandomMove) :-
    % Find all legal moves in the current state
    findall(Move, legal_move(Move, State, _), Moves),

    % Select a random move from the list of legal moves
    random_member(RandomMove, Moves).