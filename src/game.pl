:- consult('board.pl').



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
    write('Current Player: '), write(CurrentPlayer), nl,
    write('Board:\n'),
    show_board(Board).
