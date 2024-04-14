:- module(prettyprint, [print_board/1]).

% Print the board with pieces
print_board([R1, R2, R3, R4, R5, R6, R7, R8]) :-
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R1, 8),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R2, 7),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R3, 6),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R4, 5),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R5, 4),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R6, 3),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R7, 2),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    print_row(R8, 1),
    write('   +---+---+---+---+---+---+---+---+'), nl,
    write('     0   1   2   3   4   5   6   7'), nl.


% Print a single row with pieces
print_row(Row, RowNumber) :-
    NewNumber is 8 - RowNumber,
    write(' '), write(NewNumber), write(' '),
    print_pieces(Row),
    write('|'), nl.

% Print pieces in a row
print_pieces([]) :- write('').
print_pieces([Piece|Rest]) :-
    write('|'),
    format_cell(Piece),
    print_pieces(Rest).

% Format a cell with the piece (or empty)
format_cell(Piece) :-
    (   var(Piece)
    ->  write(' . ')
    ;   write(' '), write(Piece), write(' ')).