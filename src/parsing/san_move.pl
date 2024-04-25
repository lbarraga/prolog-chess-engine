:- module(san_move, [san_move/3]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').

% Module die een net geparste plie omzet in een move met een van en naar een coordinate

san_move(san(castle(Type), Check), castle(Type), State) :-
    legal_move(castle(Type), State, _),
    check(Check, State), !.

san_move(san(Plie, Check), Move, State) :-
    write(Plie), nl,
    writeln('begin plie_move'),
    plie_move(Plie, Move, State),
    write('plie done'), nl,
    check(Check, State).

plie_move(plie(PieceName, Takes, co(R, C), Disambiguation, no_promotion), move(From, (R, C)), state(StartBoard, Info, Color)) :-
    writeln('plie_move'),
    legal_move(move(From, (R, C)), state(StartBoard, Info, Color), _),
    writeln('legal_move'),
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName),   % Piece must have same name as in the plie
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece), % Takes must be correct
    disambiguate(From, Disambiguation), !.

plie_move(plie(PieceName, Takes, co(R, C), Disambiguation, Promotion), promotion(From, (R, C), Promotion), state(StartBoard, Info, Color)) :-
    legal_move(promotion(From, (R, C), Promotion), state(StartBoard, Info, Color), _),
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName),   % Piece must have same name as in the plie
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece), % Takes must be correct
    disambiguate(From, Disambiguation).

takes(takes, FromPiece, ToPiece) :-
    color(FromPiece, FromColor),
    color(ToPiece, ToColor),
    opponent(FromColor, ToColor).

takes(no_takes, _, ToPiece) :- empty(ToPiece).

check(checkmate, State) :- is_checkmate(State).
check(check, State) :- king_in_check(State).
check(no_check, State) :- \+ king_in_check(State).

disambiguate((_, Col), file(Col)).       % Disambiguation on file (e.g. Rfxe1)
disambiguate((Row, _), rank(Row)).       % Disambiguation on rank (e.g. R1xe5)
disambiguate((Row, Col), co(Row, Col)).  % Disambiguation on coordinate (e.g. Re1xe5)
disambiguate(_, none).                   % No disambiguation (e.g. Rxe5)

