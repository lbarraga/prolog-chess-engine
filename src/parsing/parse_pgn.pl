:- module(parse_pgn, [pgn/4]).

:- use_module(parse_san, [san/4]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

between(Left, Between, Right) --> Left, string_without(Right, Between), Right.

% Define the grammar for parsing PGN files.
pgn(Moves, R) --> tags(R), movetext(Moves), result, blanks.

tags(koth_rules) --> tag(koth_rules), tags(_).
tags(koth_rules) --> tag(_), tags(koth_rules).
tags(normal_rules) --> tag(normal_rules), tags(normal_rules).
tags(normal_rules) --> [].
tags(normal_rules) --> eol.

tag(koth_rules) --> "[Rules \"koth\"]", eol.
tag(normal_rules) --> between("[", _, "]"), eol.

move_nr --> digits(_), ".".
plie(san(San, Check)) --> blanks, san(San, Check), blanks.

move(San1, San2) --> move_nr, plie(San1), plie(San2).
move_plie(San) --> move_nr, plie(San).

movetext([turn(San1, San2) | Rest]) --> move(San1, San2), movetext(Rest).
movetext([turn(San, no_move)]) --> move_plie(San).
movetext([]) --> [].

result --> "1-0".
result --> "0-1".
result --> "1/2-1/2".
result --> "*".
result --> [].
