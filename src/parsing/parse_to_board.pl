:- module(parse_to_board, [parse_to_board/4]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').
:- use_module('../parsing/san_move.pl', [san_move/3]).

parse_to_board(Plies, StartState, EndState, MoveCounts) :-
    parse_to_board_(1, Plies, StartState, EndState, MoveCounts).


parse_to_board_(_, [], State, State, []).
parse_to_board_(N, [turn(WhitePlie, BlackPlie) | Rest], StartState, EndState, [CountWhite, CountBlack | RestCount]) :-
    write('Turn '), write(N), nl,
    write('White: '), writeln(WhitePlie),
    StartState = state(_, Info, _),
    writeln(Info),
    parse_plie(WhitePlie, StartState, WhiteTempState, CountWhite),
    WhiteTempState = state(WhiteTempBoard, _, _),
    print_board(WhiteTempBoard),
    write('Black: '), writeln(BlackPlie),
    WhiteTempState = state(_, BlackInfo, _),
    writeln(BlackInfo),

    parse_plie(BlackPlie, WhiteTempState, BlackTempState, CountBlack),
    nl,
    BlackTempState = state(BlackTempBoard, _, _),
    print_board(BlackTempBoard),
    N1 is N + 1,
    parse_to_board_(N1, Rest, BlackTempState, EndState, RestCount).
% 17, 21, 31, 40, 59, 89

count_moves(State, Count) :-
    findall(castle(Type), legal_move(castle(Type), State, _), Castles),
    findall(promotion(F, T, PP), legal_move(promotion(F, T, PP), State, _), Promotions),
    findall(move(F, T), legal_move(move(F, T), State, _), Moves),
    length(Castles, CastleLength),
    length(Promotions, PromotionLength),
    length(Moves, MoveLength),
    Count is CastleLength + PromotionLength // 2 + MoveLength.
%     State = state(B, Info, C),
%     print_board(B), write(' '), write(Info), write(' '), write(C), write(' '),
%     write(CastleLength), write(' '), write(PromotionLength), write(' '), write(MoveLength), write(' '), writeln(Count).

parse_plie(no_move, State, State, 0).
parse_plie(San, State, EndState, Count) :-
    count_moves(State, Count),
    writeln(Count),
    writeln(San),
    san_move(San, Move, State),
    writeln(Move),
    legal_move(Move, State, EndState).