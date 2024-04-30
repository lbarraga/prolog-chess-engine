:- module(castling, [castle/3]).
:- use_module('../pieces.pl').
:- use_module('../board.pl').
:- use_module(moves_main).

% castle(+Type, +State, -NewState)
% NewState is the state after castling of the given type in the given state.
% Type is either short_castle or long_castle.
castle(Type, state(Board, Info, Color), state(NewBoard, NewInfo, Opponent)) :-
    opponent(Color, Opponent),
    disable_castling(Color, Info, NewInfo),
    can_castle(Type, state(Board, Info, Color)),
    castling_reqs(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos, _),    % Get the positions of the king, rook and squares between them.
    move_unsafe(move(KingPos, TwoNextToKingPos), Board, TempBoard),                         % Place king two squares towards rook.
    move_unsafe(move(RookPos, OneNextToKingPos), TempBoard, NewBoard).                      % Place rook next to king.


% can_castle(+Type, +State)
% True if the player in the given state can castle in the given direction.
can_castle(Type, state(Board, info(Castling, _), Color)) :-
    \+ king_in_check(state(Board, _, Color)),
    castling_reqs(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos, Castling),     % Get the positions of the king, rook and squares between them.
    co_empty(OneNextToKingPos, Board), co_empty(TwoNextToKingPos, Board),                           % Squares between king and rook are empty.
    legal_move(move(RookPos, OneNextToKingPos), state(Board, _, Color), _),                         % Rook could move next to king.
    legal_move(move(KingPos, OneNextToKingPos), state(Board, _, Color), state(TempBoard, _, _)),    % King could move one square closer to rook.
    legal_move(move(OneNextToKingPos, TwoNextToKingPos), state(TempBoard, _, Color), _).            % King could move two squares closer to rook.


% castling_reqs(+Color, +Type, -KingPos, -OneNextToKingPos, -TwoNextToKingPos, -RookPos, -Castling)
% Get the positions of the king, rook and squares between them for the given color and castling type.
%                           |King| |King+1||King+2| |Rook|
castling_reqs(white, long,  (7, 4), (7, 3), (7, 2), (7, 0), castling_info((long, _), _)).
castling_reqs(white, short, (7, 4), (7, 5), (7, 6), (7, 7), castling_info((_, short), _)).
castling_reqs(black, long,  (0, 4), (0, 3), (0, 2), (0, 0), castling_info(_, (long, _))).
castling_reqs(black, short, (0, 4), (0, 5), (0, 6), (0, 7), castling_info(_, (_, short))).

% disable_castling(+Color, +Info, -NewInfo)
% Disable castling for the given color in the given board info.
disable_castling(white, info(castling_info(_, Black), _), info(castling_info((no_long, no_short), Black), no_ep)).
disable_castling(black, info(castling_info(White, _), _), info(castling_info(White, (no_long, no_short)), no_ep)).
