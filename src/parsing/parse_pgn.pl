:- module(pgn_parser, [parse_pgn_file/1]).

% Entry point for parsing a PGN file.
parse_pgn --> optional_tags(Tags), moves(Moves), { construct_pgn(Tags, Moves, PGN) }, eos, { writeln(PGN) }.

% Rule for parsing optional tags followed by an empty line.
optional_tags(Tags) --> tags(Tags), blanks; { Tags = [] }.

% Rule for parsing a sequence of tags.
tags([Tag|Tags]) --> tag(Tag), blanks, tags(Tags).
tags([]) --> [].

% Rule for parsing individual tags.
tag(white(Name)) --> "[White \"", string(Name), "\"]".
tag(black(Name)) --> "[Black \"", string(Name), "\"]".
tag(rules(Rules)) --> "[Rules \"", string(Rules), "\"]".
tag(result(Result)) --> "[Result \"", string(Result), "\"]".

% Helper rule for parsing a quoted string.
string(Str) --> string_inner(StrChars), { atom_chars(Str, StrChars) }.

% Helper rule for parsing the characters inside a string.
string_inner([Char|Chars]) --> [Char], { Char \= '"' }, string_inner(Chars).
string_inner([]) --> [].

% Rule for parsing the movetext section, which can include comments.
moves(Moves) --> move_sequence(Moves).

% Rule for parsing a sequence of moves with optional comments.
move_sequence([Move|Moves]) --> move(Move), blanks, !, move_sequence(Moves).
move_sequence([]) --> [].

% Rule for an individual move, including optional comments.
move(move(Move, Comment)) --> top_move(Move), optional_comment(Comment).

% Placeholder for 'top_move' - this should be replaced with your actual move parsing DCG.
top_move(Move) --> [M], { char_type(M, alpha) }, { atom_chars(Move, [M]) }.

% Optional comment parsing.
optional_comment(Comment) --> "{", string_inner(CommentChars), "}", { atom_chars(Comment, CommentChars) }; { Comment = '' }.

% Helper rule for end of string.
eos([], []).

% Construct the PGN representation from parsed tags and moves.
construct_pgn(Tags, Moves, PGN) :- PGN = pgn{tags: Tags, moves: Moves}.

% Wrapper predicate for phrase_from_file
parse_pgn_file(FilePath) :-
    phrase_from_file(parse_pgn, FilePath).

% Helper rules for whitespace.
blanks --> blank, blanks.
blanks --> [].

blank --> [C], { char_type(C, space) ; C == '\n' }.
