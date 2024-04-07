:- module(parse_pgn, [pgn/2]).

:- use_module(parse_san, [san/2]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

between(Left, Between, Right) --> Left, string_without(Right, Between), Right.

% Define the grammar for parsing PGN files.
pgn --> tags, movetext, result, blanks.

tags --> tag, tags.
tags --> [].
tags --> eol.

tag --> between("[", _, "]"), eol.

move_nr --> digits(_), ".".
plie --> blanks, san, blanks.

move --> move_nr, plie, plie.
move_plie --> move_nr, plie.

movetext --> move, movetext.
movetext --> move_plie.
movetext --> [].

result --> "1-0".
result --> "0-1".
result --> "1/2-1/2".
result --> [].
