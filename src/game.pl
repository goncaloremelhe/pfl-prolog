:- consult('board.pl').

print_Coords(Length, Length) :- 
    write(' '),
    write(Length),
    nl, nl.
print_Coords(CurrIndex, Length):-
    write(' '),
    write(CurrIndex),
    NewIndex is CurrIndex+1,
    print_Coords(NewIndex, Length).

show_topCoords([Row|_]):-
    length(Row, Length),
    write('  '),
    print_Coords(1, Length).

% ----------- game_loop(+GameConfig)
% starts the game loop with given config
game_loop(GameConfig):-
    initial_state(GameConfig, GameState),
    display_game(GameState).


% ----------- initial_state(+GameConfig)
% Receives game configuration and returns the initial game state (player with the black pieces is starting for now but we can change this later)
initial_state([GameMode, BoardSize], [Board, black, GameMode]) :-
    generate_board(BoardSize, Board).

% ----------- display_game(+GameState)
% Receives game state and displays current player and board
display_game([Board, CurrentPlayer, GameMode]):-
    write('\nCurrent Player: '), write(CurrentPlayer), nl,
    write('Board:\n\n'),
    show_topCoords(Board),
    show_board(Board, 'a').
