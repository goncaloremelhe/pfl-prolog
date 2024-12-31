:- consult('board.pl').


% player with the black pieces starts
initial_state([],[]).
initial_state([GameMode, BoardSize], [Board, black, GameMode]) :-
    generate_board(BoardSize, Board).


display_game([Board, CurrentPlayer, GameMode]):-
    write('Current Player: '), write(CurrentPlayer), nl,
    write('Board:\n'),
    show_board(Board).

/*
game(Input) :-
    write('ola').
    */
