% Run tests on pairs of PGN and FEN files
run_tests :-
    test(Files),
    maplist(test_pgn_fen_pair, Files).

% Trim leading and trailing whitespaces
trim(String, Trimmed) :-
    string_codes(String, Codes),
    drop_space(Codes, NoLeading),
    reverse(NoLeading, Reversed),
    drop_space(Reversed, NoTrailingReversed),
    reverse(NoTrailingReversed, NoTrailing),
    string_codes(Trimmed, NoTrailing).

% Drop leading spaces
drop_space([32|T], Result) :- !, drop_space(T, Result).
drop_space(Result, Result).

% Test a single pair of PGN and FEN files, with paths prepended, error handling, and colored output
test_pgn_fen_pair((PgnFile, FenFile, MoveFile)) :-
    % Prepend directory paths to filenames
    atom_concat('tests/extra/test_pgns/', PgnFile, FullPgnPath),
    atom_concat('tests/extra/test_fens/', FenFile, FullFenPath),
    atom_concat('tests/extra/test_move_counts/', MoveFile, FullMovePath),
    % Use full paths to parse the PGN file
    init_state(StartState),
    parse_file(FullPgnPath, Moves),

    % Attempt to parse moves into a board, handle failure
    ( parse_to_board(Moves, StartState, _, Counts),
      % Read and trim the expected FEN
      read_fen_from_file(FullFenPath, ExpectedFenRaw),
      trim(ExpectedFenRaw, ExpectedFen),

      % Compare and print results with color
      ( GeneratedFen = ExpectedFen ->
          % Check if move counts match
          read_move_counts_from_file(FullMovePath, ExpectedCounts),
          ( Counts = ExpectedCounts ->
              ansi_format([fg(green)], 'PASS: FENs and move counts match for ~w~n', [FullPgnPath])
          ;
              ansi_format([fg(red)], '[FAIL]: Move counts do not match for ~w~n', [FullPgnPath]),
              ansi_format([fg(white)], '\tExpected Counts:  ~w~n', [ExpectedCounts]),
              ansi_format([fg(white)], '\tGenerated Counts: ~w~n', [Counts])
          )
      ;
          ansi_format([fg(red)], '[FAIL]: FENs do not match for ~w~n', [FullPgnPath]),
          ansi_format([fg(white)], '\tExpected:  ~w~n', [ExpectedFen]),
          ansi_format([fg(white)], '\tGenerated: ~w~n', [GeneratedFen])
      )
    ;
        ansi_format([fg(yellow)], '[ERROR]: Failed to parse board from PGN ~w~n', [FullPgnPath])
    ).

% Read move counts from a file
read_move_counts_from_file(FullPath, Counts) :-
    catch(
        read_file_to_string(FullPath, FileContent, []),
        _,
        (write('Error reading move counts file: '), writeln(FullPath), fail)
    ),
    trim(FileContent, Trimmed),
    split_string(Trimmed, " ", "", CountsStr),
    maplist(atom_number, CountsStr, Counts).

% Read the expected FEN string from a FEN file
read_fen_from_file(FenFile, Fen) :-
    open(FenFile, read, Stream),
    read_line_to_string(Stream, Fen),
    close(Stream).


list_directory(Dir, Files) :- directory_files(Dir, [_, _ | Files]).

zip([], [], [], []).
zip([H1|T1], [H2|T2], [H3|T3], [(H1, H2, H3)|T]) :- zip(T1, T2, T3, T).

test(Files) :-
    list_directory('tests/extra/test_pgns', PgnFiles),
    list_directory('tests/extra/test_fens', FenFiles),
    list_directory('tests/extra/test_move_counts', MoveCountFiles),
    zip(PgnFiles, FenFiles, MoveCountFiles, Files).


% Convert board to FEN string
board_to_fen(state(Board, _, _), FEN) :-
    maplist(row_to_fen_string, Board, FENParts),
    atomic_list_concat(FENParts, '/', FEN).

% Convert a single row to a FEN string part
row_to_fen_string(Row, FENString) :-
    compress_row(Row, CompressedRow),
    atomic_list_concat(CompressedRow, FENString).

% Compress a row by counting consecutive empty spaces
compress_row([], []).
compress_row([' '|T], Result) :-
    !, count_spaces(T, 1, Rest, Count),
    number_chars(Count, CountChars),
    compress_row(Rest, RestResult),
    append(CountChars, RestResult, Result).
compress_row([H|T], [H|RestResult]) :-
    compress_row(T, RestResult).

% Count consecutive empty spaces in a row
count_spaces([' '|T], Acc, Rest, Count) :-
    !, NewAcc is Acc + 1,
    count_spaces(T, NewAcc, Rest, Count).
count_spaces(Rest, Count, Rest, Count).
