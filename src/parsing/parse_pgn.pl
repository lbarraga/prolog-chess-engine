:- module(parse_pgn, [pgn/4]).

:- use_module(parse_san, [san/4]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% Parse a string between two other strings.
between(Left, Between, Right) --> Left, string_without(Right, Between), Right.

% Define the grammar for parsing PGN files.
pgn(Moves, R) --> tags(R), movetext(Moves), result, blanks.

% Parse the tags. If koth is present in the current parsed tag or in the other tags, the rules are koth.
tags(koth_rules) --> tag(koth_rules), tags(_).
tags(koth_rules) --> tag(_), tags(koth_rules).
tags(normal_rules) --> tag(normal_rules), tags(normal_rules).
tags(normal_rules) --> [].
tags(normal_rules) --> eol.

% Parse a single tag. We only care about a possible koth tag.
tag(koth_rules) --> "[Rules \"koth\"]", eol.
tag(normal_rules) --> between("[", _, "]"), eol.

move_nr --> digits(_), ".".
ply(san(San, Check)) --> blanks, san(San, Check), blanks.

move(San1, San2) --> move_nr, ply(San1), ply(San2).
move_ply(San) --> move_nr, ply(San).

% Parse the movetext as a list of ply tuples.
movetext([turn(San1, San2) | Rest]) --> move(San1, San2), movetext(Rest).
movetext([turn(San, no_move)]) --> move_ply(San).
movetext([]) --> [].

result --> "1-0".
result --> "0-1".
result --> "1/2-1/2".
result --> "*".
result --> [].
