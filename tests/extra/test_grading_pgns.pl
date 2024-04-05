:- module(test_grading_pgns, [parse_pgn_test/1]).
:- use_module("../../src/parsing/parse_pgn").

file_contains(FileName, Codes) :-
    open(FileName, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream).

test_pgn_file(File) :-
    file_contains(File, Codes),
    (   phrase(pgn(), Codes)
    ->  format('[OK] ~w~n', [File]), true  % Continue if parsing is successful
    ;   format('[FAIL] ~w~n', [File]), fail  % Halt with error if parsing fails
    ).

test_all_pgn_files(Directory, [File|RestFiles]) :-
    test_single_pgn(Directory, File),  % Test the first file
    !,  % If the test is successful, proceed
    test_all_pgn_files(Directory, RestFiles).  % Continue with the rest of the files

test_all_pgn_files(_, []).  % Base case: no more files to test

test_single_pgn(Directory, File) :-
    atom_concat(Directory, '/', DirSlash),
    atom_concat(DirSlash, File, FilePath),
    test_pgn_file(FilePath).

parse_pgn_test(Directory) :-
    directory_files(Directory, Files),
    exclude([File]>>sub_atom(File, 0, _, _, '.'), Files, FilesNoDots),  % Exclude hidden files and directories
    test_all_pgn_files(Directory, FilesNoDots),
    halt(0).  % Halt normally after all files are tested successfully

