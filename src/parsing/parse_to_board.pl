:- module(parse_to_board, [parse_to_board/3]).

parse_to_board(Plies, StartBoard, EndBoard) :-
    parse_to_board_(1, Plies, StartBoard, EndBoard).

parse_to_board_(_, [], Board, Board).
parse_to_board_(N, [turn(WhitePlie, BlackPlie) | Rest], StartBoard, EndBoard) :-
%     write('Turn '), write(N), nl,
%     write('White: '), writeln(WhitePlie),
    parse_plie(white, WhitePlie, StartBoard, WhiteTempBoard),
%     print_board(WhiteTempBoard),
%     write('Black: '), writeln(BlackPlie),
    parse_plie(black, BlackPlie, WhiteTempBoard, BlackTempBoard),
%     print_board(BlackTempBoard),
    N1 is N + 1,
    parse_to_board_(N1, Rest, BlackTempBoard, EndBoard).
% 17, 21, 31, 40, 59, 89

parse_plie(_, no_move, Board, Board).
parse_plie(Color, castle(Type), StartBoard, EndBoard) :-
    uber_move_castling(
        Type,
        Color,
        StartBoard,
        info(can_castle_short, can_castle_long),
        EndBoard,
        info(can_not_castle_short, can_not_castle_long)
    ).

parse_plie(Color, plie(PieceName, co(R, C), Disambiguation, Promotion), StartBoard, EndBoard) :-
    uber_move_promotion(From, (R, C), Color, Promotion, StartBoard, EndBoard),   % All Coordinates of piece that can move to destination
    get(From, StartBoard, Piece), name(Piece, PieceName),   % Piece must have same name as in the plie
    disambiguate(From, Disambiguation).                     % Disambiguate the piece

disambiguate((_, Col), file(Col)).       % Disambiguation on file (e.g. Rfxe1)
disambiguate((Row, _), rank(Row)).       % Disambiguation on rank (e.g. R1xe5)
disambiguate((Row, Col), co(Row, Col)).  % Disambiguation on coordinate (e.g. Re1xe5)
disambiguate(_, none).                   % No disambiguation (e.g. Rxe5)

