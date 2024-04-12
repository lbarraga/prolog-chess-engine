:- module(parse_to_board, [parse_to_board/3]).

parse_to_board(Plies, StartState, EndState) :-
    parse_to_board_(1, Plies, StartState, EndState).

parse_to_board_(_, [], State, State).
parse_to_board_(N, [turn(WhitePlie, BlackPlie) | Rest], StartState, EndState) :-
%     write('Turn '), write(N), nl,
%     write('White: '), writeln(WhitePlie),
%     StartState = state(_, Info, _),
%     writeln(Info),

    parse_plie(WhitePlie, StartState, WhiteTempState),

%     WhiteTempState = state(WhiteTempBoard, _, _),
%     print_board(WhiteTempBoard),
%     write('Black: '), writeln(BlackPlie),
%     WhiteTempState = state(_, BlackInfo, _),
%     writeln(BlackInfo),

    parse_plie(BlackPlie, WhiteTempState, BlackTempState),

%     BlackTempState = state(BlackTempBoard, _, _),
%     print_board(BlackTempBoard),
    N1 is N + 1,
    parse_to_board_(N1, Rest, BlackTempState, EndState).
% 17, 21, 31, 40, 59, 89

parse_plie(no_move, State, State).

parse_plie(castle(Type), StartState, EndState) :- legal_move(castle(Type), StartState, EndState).

parse_plie(plie(PieceName, co(R, C), Disambiguation, Promotion), state(StartBoard, Info, Color), EndState) :-
    legal_move(promotion(From, (R, C), Promotion), state(StartBoard, Info, Color), EndState),   % All Coordinates of piece that can move to destination
    get(From, StartBoard, Piece), name(Piece, PieceName),   % Piece must have same name as in the plie
    disambiguate(From, Disambiguation).                     % Disambiguate the piece

disambiguate((_, Col), file(Col)).       % Disambiguation on file (e.g. Rfxe1)
disambiguate((Row, _), rank(Row)).       % Disambiguation on rank (e.g. R1xe5)
disambiguate((Row, Col), co(Row, Col)).  % Disambiguation on coordinate (e.g. Re1xe5)
disambiguate(_, none).                   % No disambiguation (e.g. Rxe5)

