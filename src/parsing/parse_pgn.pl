:- module(parse_pgn, [pgn/2]).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% Define the grammar for parsing PGN files.
pgn --> tags, movetext, result, blanks.

tags --> tag, tags.
tags --> [].
tags --> eol.

tag --> "[", string_without("]", _), "]", eol.

move_nr --> digits(_), ".".
plie --> blanks, nonblanks(_), blanks.

move --> move_nr, plie, plie.
move_plie --> move_nr, plie.

movetext --> move, movetext.
movetext --> move_plie.
movetext --> [].

result --> "1-0".
result --> "0-1".
result --> "1/2-1/2".
result --> [].
