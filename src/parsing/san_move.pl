:- module(san_move, [san_move/3]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').

% This module converts the parsed ply/san move (san(ply, check)) to a move
% that can be used internally in the engine (move(from, to)).

% san_move(?San, ?Move, +State)
% San is the parsed ply/san move
% Move is the internal move representation
% State is the current state of the game
san_move(no_move, no_move, _).

% castle stays the same, but we need to check if the move is legal and the check is correct.
san_move(San, castle(Type), State) :- san_move_(San, castle(Type), State), !.

% promotion stays the same, but we need to check if the move is legal and the check is correct.
san_move(San, promotion(From, To, Promotion), State) :- san_move_(San, promotion(From, To, Promotion), State), !.

% For all other moves we need to check if the move is legal and the check is correct.
% We also need to disambiguate the move if there are multiple pieces that can make the move.
san_move(San, move(From, To), State) :-
    san_move_(San, move(From, To), State),
    State = state(Board, _, _),
    % get disambiguation
    San = san(ply(_, Takes, _, Dis, _), _),
    get(From, Board, Piece),
    findall(
        F,
        (legal_move(move(F, To), State, _), get(F, Board, Piece), disambiguate(Piece, Takes, F, Dis)),
        [_] % There should only one possible move left after disambiguation
    ),
    !.

% san_move_(+San, +Move, +State)
% This predicate checks if the castle move is legal and the check is correct.
san_move_(san(castle(Type), Check), castle(Type), State) :-
    legal_move(castle(Type), State, NextState),
    check(Check, NextState), !.

% This predicate checks if the ply move is legal
san_move_(san(Ply, Check), Move, State) :-
    ply_move(Ply, Check, Move, State).

% ply_move(+Ply, +Check, -Move, +State)
% Convert ply to internal move
ply_move(ply(PieceName, Takes, co(R, C), Disambiguation, no_promotion), Check, move(From, (R, C)), state(StartBoard, Info, Color)) :-
    legal_move(move(From, (R, C)), state(StartBoard, Info, Color), NextState), % Check if the move is legal
    check(Check, NextState), % Check if the check is correct
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName), % Get the piece that is moving
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece, Info, co(R, C)), % Check if takes is correct
    disambiguate(PieceName, Takes, From, Disambiguation). % Disambiguate the move

% promoting move
ply_move(ply(PieceName, Takes, co(R, C), Disambiguation, Promotion), Check, promotion(From, (R, C), Promotion), state(StartBoard, Info, Color)) :-
    legal_move(promotion(From, (R, C), Promotion), state(StartBoard, Info, Color), NextState),
    check(Check, NextState),
    get(From, StartBoard, FromPiece), name(FromPiece, PieceName),
    get((R, C), StartBoard, ToPiece), takes(Takes, FromPiece, ToPiece, Info, co(R, C)),
    disambiguate(PieceName, Takes, From, Disambiguation).

% takes(+Takes, +FromPiece, +ToPiece, +Info, +To)
% Check if the move correctly does or does not take a piece.
takes(takes, FromPiece, _, info(_, (R, C)), co(R, C)) :- name(FromPiece, pawn), !. % En passant takes can have empty take square.

% if the san specifies a capture, the opposing square must be occupied by an enemy piece
takes(takes, FromPiece, ToPiece, _, _) :-
    color(FromPiece, FromColor),
    color(ToPiece, ToColor),
    opponent(FromColor, ToColor).

% if the san does not specify a capture, the opposing square must be empty
takes(no_takes, _, ToPiece, _, _) :- empty(ToPiece).

% check(+Check, +State)
% Check if the check is correct
check(checkmate, State) :- is_checkmate(State).
check(check, State) :- king_in_check(State).
check(no_check, State) :- \+ king_in_check(State).

% disambiguate(+Piece, +Takes, +From, -Disambiguation)
% Disambiguate the move if there are multiple pieces that can make the move.
disambiguate(pawn, takes, (_, Col), file(Col)) :- !.    % No disambiguation for pawns
disambiguate(_, _, _, none).                            % No disambiguation (e.g. Rxe5)
disambiguate(_, _, (_, Col), file(Col)).                % Disambiguation on file (e.g. Rfxe1)
disambiguate(_, _, (Row, _), rank(Row)).                % Disambiguation on rank (e.g. R1xe5)
disambiguate(_, _, (Row, Col), co(Row, Col)).           % Disambiguation on coordinate (e.g. Re1xe5)

