:- module(san_move, [san_move/3]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').

% Module die een net geparste plie omzet in een move met een van en naar een coordinate

san_move(no_move, no_move, _).
san_move(San, castle(Type), State) :- san_move_(San, castle(Type), State), !.
san_move(San, promotion(From, To, Promotion), State) :- san_move_(San, promotion(From, To, Promotion), State), !.
san_move(San, move(From, To), State) :-
    san_move_(San, move(From, To), State),
    % only one piece can make the move
    State = state(Board, _, _),
    % get disambiguation
    San = san(plie(_, Takes, _, Dis, _), _),
    get(From, Board, Piece),
    findall(
        F,
        (legal_move(move(F, To), State, _), get(F, Board, Piece), disambiguate(Piece, Takes, F, Dis)),
        L
    ),
    L = [_],
    !.

san_move_(san(castle(Type), Check), castle(Type), State) :-
    legal_move(castle(Type), State, NextState),
    check(Check, NextState), !.

san_move_(san(Plie, Check), Move, State) :-
    plie_move(Plie, Check, Move, State).

plie_move(plie(PieceName, Takes, co(R, C), Disambiguation, no_promotion), Check, move(From, (R, C)), state(StartBoard, Info, Color)) :-
    legal_move(move(From, (R, C)), state(StartBoard, Info, Color), NextState),
    check(Check, NextState),
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName),   % Piece must have same name as in the plie
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece, Info, co(R, C)), % Takes must be correct
    disambiguate(PieceName, Takes, From, Disambiguation).

plie_move(plie(PieceName, Takes, co(R, C), Disambiguation, Promotion), Check, promotion(From, (R, C), Promotion), state(StartBoard, Info, Color)) :-
    legal_move(promotion(From, (R, C), Promotion), state(StartBoard, Info, Color), NextState),
    check(Check, NextState),
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName),   % Piece must have same name as in the plie
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece, Info, co(R, C)), % Takes must be correct
    disambiguate(PieceName, Takes, From, Disambiguation).

takes(takes, FromPiece, _, info(_, (R, C)), co(R, C)) :- name(FromPiece, pawn), !.
takes(takes, FromPiece, ToPiece, _, _) :-
    color(FromPiece, FromColor),
    color(ToPiece, ToColor),
    opponent(FromColor, ToColor).

takes(no_takes, _, ToPiece, _, _) :- empty(ToPiece).

check(checkmate, State) :- is_checkmate(State).
check(check, State) :- king_in_check(State).
check(no_check, State) :- \+ king_in_check(State).

disambiguate(pawn, takes, (_, Col), file(Col)) :- !.    % No disambiguation for pawns
disambiguate(_, _, _, none).                            % No disambiguation (e.g. Rxe5)
disambiguate(_, _, (_, Col), file(Col)).                % Disambiguation on file (e.g. Rfxe1)
disambiguate(_, _, (Row, _), rank(Row)).                % Disambiguation on rank (e.g. R1xe5)
disambiguate(_, _, (Row, Col), co(Row, Col)).           % Disambiguation on coordinate (e.g. Re1xe5)

