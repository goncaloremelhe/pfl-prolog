:- use_module(library(lists)).
:-consult('utils.pl').
:- consult('board.pl').

play :-
    write('-------------------------------------\n\n'),
    write('Mabula :))\n\n'),
    write('-------------------------------------\n'),
    write('Pick a game mode!!!!\n'),
    write('1. Person vs Person\n'),
    write('2. Person vs Computer\n'),
    write('3. Computer vs Person\n'),
    write('4. Computer vs Computer\n'),
    valid_input([1,2,3,4], GameMode),

    difficultyLevel(GameMode, Level),

    write('\nPick the number of pieces per side of the board (6, 8 or 10):\n'),
    valid_input([6, 8, 10], PiecesPerSide),
    BoardSize is PiecesPerSide + 2,
    game([GameMode, BoardSize, Level]).


% Por agora nao faz nada
difficultyLevel(1,0).
difficultyLevel(GameMode, Level):-
    GameMode > 1,
    write('\nChoose the level difficulty:\n'),
    write('1. Easy\n'),
    write('2. Hard\n'),
    valid_input([1,2], Level).


game(GameConfig):-
    initial_state(GameConfig, GameState),
    game_loop(GameConfig, GameState).


% ----------- game_loop(+GameConfig, +GameState)
% starts the game loop with given config
game_loop([1, _, _], GameState):-
    display_game(GameState),
    get_remaining_pieces(GameState, RemainingPieces),
    write('Available pieces:\n'),
    print_remaining_pieces(RemainingPieces),
    convert_to_options(RemainingPieces, Options),
    valid_input_options(Options, SelectedPiece),
    format('You chose: ~w\n', [SelectedPiece]).


% ----------- initial_state(+GameConfig)
% Receives game configuration and returns the initial game state (player with the black pieces is starting for now but we can change this later)
initial_state([GameMode, BoardSize, _], [Board, black, GameMode]) :-
    generate_board(BoardSize, Board).

% ----------- display_game(+GameState)
% Receives game state and displays current player and board
display_game([Board, CurrentPlayer, _]):-
    write('\nCurrent Player: '), write(CurrentPlayer), nl,
    write('Board:\n\n'),
    length(Board, Length),
    show_board(Board, Length),
    write('\n  '),
    print_Coords(1, Length).

% ----------- get_remaining_pieces(+GameState, -RemainingPieces)
% Determines the pieces on the perimeter that match the CurrentPlayer
get_remaining_pieces([Board, CurrentPlayer, _], RemainingPieces) :-
    get_perimeter_positions(Board, Perimeter),
    include(matches_piece(Board, CurrentPlayer), Perimeter, RemainingPiecesUnordered),
    sort(RemainingPiecesUnordered, RemainingPieces).

% ----------- get_perimeter_positions(+Board, -Perimeter)
% Returns the positions of the perimeter of the board
get_perimeter_positions(Board, Perimeter) :-
    length(Board, Size),

    % Bottom row (Y = 1)
    findall((1, Col), between(1, Size, Col), Bottom),

    % Top row (Y = Size)
    findall((Size, Col), between(1, Size, Col), Top),

    % Left column (X = 1, excluding corners)
    findall((Row, 1), between(2, Size - 1, Row), Left),

    % Right column (X = Size, excluding corners)
    findall((Row, Size), between(2, Size - 1, Row), Right),

    append([Bottom, Top, Left, Right], Perimeter).

% ----------- matches_piece(+Board, +Player, +Position)
% Checks if the piece at the given position matches the CurrentPlayer
matches_piece(Board, Player, (Row, Col)) :-
    length(Board, Size),       
    Y is Size - Row + 1,
    nth1(Y, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell = Player.

% ----------- convert_to_options(+Tuples, -Options)
% Converts a list of (Row, Column) tuples into a list of Row-Col atoms
convert_to_options([], []).
convert_to_options([(Row, Col)|RestTuples], [Option|RestOptions]) :-
    number_codes(Row, RowCodes),
    number_codes(Col, ColCodes),
    append(RowCodes, [45|ColCodes], OptionCodes),
    atom_codes(Option, OptionCodes),
    convert_to_options(RestTuples, RestOptions).

% ----------- print_Coords(+CurrIndex, +Length)
% Print the coordinates from 1 to Length
print_Coords(Length, Length) :- 
    write(' '),
    write(Length),
    nl, nl.
print_Coords(CurrIndex, Length):-
    write(' '),
    write(CurrIndex),
    NewIndex is CurrIndex+1,
    print_Coords(NewIndex, Length).

% ----------- print_remaining_pieces(+Pieces)
% Print available pieces to move in the format row-column
print_remaining_pieces([]) :- nl, nl.
print_remaining_pieces([(Row,Col)|RemainingPieces]) :-
    format('~w-~w ', [Row, Col]),
    print_remaining_pieces(RemainingPieces).


