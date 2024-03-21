playing_depth(2).

play(Board) :-
    playing_depth(Depth),
    best_move(Board, white, Depth, vm(_, move(FromWhite, ToWhite))),
    write("Move found for White: ("), write(FromWhite), write(" -> "), write(ToWhite), write(")"), nl,
    move(FromWhite, ToWhite, Board, BlackBoard),
    print_board(BlackBoard), nl, 
    best_move(BlackBoard, black, Depth, vm(_, move(FromBlack, ToBlack))),
    write("Move found for Black: ("), write(FromBlack), write(" -> "), write(ToBlack), write(")"), nl,
    move(FromBlack, ToBlack, BlackBoard, WhiteBoard),
    print_board(WhiteBoard), nl,
    play(WhiteBoard).