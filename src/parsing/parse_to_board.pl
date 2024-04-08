:- module(parse_to_board, [parse_to_board/3]).

parse_to_board([turn(WhitePlie, BlackPlie) | Rest], StartBoard, EndBoard) :-
    write('White: '),
    parse_plie(white, WhitePlie, StartBoard, WhiteTempBoard),
    write('Black: '),
    parse_plie(black, BlackPlie, WhiteTempBoard, BlackTempBoard),
    parse_to_board(Rest, BlackTempBoard, EndBoard).


parse_plie(Color, plie(PieceName, co(R, C), Disambiguation), StartBoard, EndBoard) :-
    writeln(co(R, C)),
    uber_move(From, (R, C), Color, StartBoard, EndBoard),   % All Coordinates of piece that can move to destination
    writeln(From),
    get(From, Board, Piece),
    name(Piece, Name).         % Piece must have same name as in the plie
    writeln(Name),
    disambiguate(From, Disambiguation).                     % Disambiguate the piece

disambiguate((_, Col), file(Col)).       % Disambiguation on file (e.g. Rfxe1)
disambiguate((Row, _), rank(Row)).       % Disambiguation on rank (e.g. R1xe5)
disambiguate((Row, Col), co(Row, Col)).  % Disambiguation on coordinate (e.g. Re1xe5)
disambiguate(_, none).                   % No disambiguation (e.g. Rxe5)

