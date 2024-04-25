:- module(san_move, [san_move/3]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').

% Module die een net geparste plie omzet in een move met een van en naar een coordinate

san_move(San, castle(Type), State) :- san_move_(San, castle(Type), State), !.
san_move(San, move(From, To), State) :-
    san_move_(San, move(From, To), State),
    % only one piece can make the move
    State = state(Board, _, _),
    % get disambiguation
    San = san(plie(_, _, _, Dis, _), _),
    writeln(San),
    get(From, Board, Piece),
    writeln(Piece), nl,
    findall(
        F,
        (legal_move(move(F, To), State, _), get(F, Board, Piece), disambiguate(F, Dis)),
        [_]
    ), !.

san_move_(san(castle(Type), Check), castle(Type), State) :-
    legal_move(castle(Type), State, _),
    check(Check, State), !.

san_move_(san(Plie, Check), Move, State) :-
    plie_move(Plie, Move, State),
    check(Check, State).

plie_move(plie(PieceName, Takes, co(R, C), Disambiguation, no_promotion), move(From, (R, C)), state(StartBoard, Info, Color)) :-
    legal_move(move(From, (R, C)), state(StartBoard, Info, Color), _),
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName),   % Piece must have same name as in the plie
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece), % Takes must be correct
    disambiguate(From, Disambiguation).

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

disambiguate(_, none).                   % No disambiguation (e.g. Rxe5)
disambiguate((_, Col), file(Col)).       % Disambiguation on file (e.g. Rfxe1)
disambiguate((Row, _), rank(Row)).       % Disambiguation on rank (e.g. R1xe5)
disambiguate((Row, Col), co(Row, Col)).  % Disambiguation on coordinate (e.g. Re1xe5)

