:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('utils.pl').
:- consult('board.pl').

% ----------- play
% Initiates the game by displaying welcome messages and prompts the user to select the game mode,
% difficulty level, board size, and scoring system. Validates the user inputs and starts the game loop
% with the configured settings.
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

    write('\nPick the number of pieces per side of the board (from 5 to 9):\n'),
    valid_input([5,6,7,8,9], PiecesPerSide),
    BoardSize is PiecesPerSide + 2,
    
    write('\nPick the scoring system (standard or product):\n'),
    write('1. Standard\n'),
    write('2. Product\n'),
    valid_input([1,2], ScoreSystem),
    game([GameMode, BoardSize, Level, ScoreSystem]).

% ----------- difficultyLevel(+GameMode, -Level)
% Asks user which difficulty level they want the computer to have, stores result in Level
difficultyLevel(1,0).
difficultyLevel(GameMode, Level):-
    GameMode > 1,
    write('\nChoose the level difficulty:\n'),
    write('1. Easy\n'),
    write('2. Hard\n'),
    valid_input([1,2], Level).

% game(+GameState)
% Receives game config and builds the game's initial state, also starts the game loop by providing the game state and list of all available moves
game([GameMode, BoardSize, Level, ScoreSystem]):-
    initial_state([GameMode, BoardSize, Level, ScoreSystem], GameState),
    valid_moves(GameState, ListOfMoves),
    game_loop([GameMode, BoardSize, Level], GameState, 'DNF', ListOfMoves).

% ----------- getMovesFromValidMoves(+ListOfMoves, -Moves)
% Receives the ListOfMoves for a Player and output a list of the moves they can use in a format column-row
getMovesFromValidMoves([], '').
getMovesFromValidMoves([(Piece,Move)|_], Piece, Move).
getMovesFromValidMoves([_|Moves], Piece, Move) :-
    getMovesFromValidMoves(Moves, Piece, Move).


% ----------- getMovesFromValidMoves(+ListOfMoves, -Pieces)
% Receives the ListOfMoves for a Player and output a list of the pieces they can use in a format column-row
getPiecesFromValidMoves([], []).
getPiecesFromValidMoves([(Piece, _)|Moves], [Piece|Pieces]):-
    getPiecesFromValidMoves(Moves,Pieces).


% ----------- game_loop(+GameConfig, +GameState, +CurrentResult, +ListOfMoves)
% Manages the main game loop, handling player turns, updating game state, and determining game progression.
%
% Strategy:
% - Continuously loop while there are available moves and the game is not in a terminal state.
% - Display the current game board and available moves to the player.
% - Prompt the player to select a piece to move from the available options.
% - Validate the player's input to ensure it corresponds to a valid move.
% - Update the game state based on the selected move and check for game termination conditions.
% - If a User does not have moves, but the next player has, pass their turn
game_loop([2, BoardSize, Level], GameState, 'DNF', ListOfMoves) :-
    ListOfMoves \= [],
    GameState = [_, black, _],
    !,

    display_game(GameState),

    write('Available pieces:\n'),
    getPiecesFromValidMoves(ListOfMoves, RemainingPieces),
    print_options(RemainingPieces),
    write('Choose a piece to move (e.g., 1-2):\n'),
    valid_input_options(RemainingPieces, SelectedPiece),
    format('You chose: ~w\n', [SelectedPiece]),

    write('Possible moves for this piece:\n'), 
    getMovesFromValidMoves(ListOfMoves, SelectedPiece, MoveOptions),
    print_options(MoveOptions),
    write('Choose a move to that piece (e.g., 1-2):\n'),
    valid_input_options(MoveOptions, SelectedMove),
    format('You chose: ~w\n', [SelectedMove]),

    % Fazer o movimento para essa peça
    move(GameState, (SelectedPiece, SelectedMove), NewGameState),
    game_over(NewGameState, Winner),
    valid_moves(NewGameState, NextMoves),
    game_loop([2, BoardSize, Level], NewGameState, Winner, NextMoves).

game_loop([2, BoardSize, Level], GameState, 'DNF', ListOfMoves) :-
    ListOfMoves \= [],
    GameState = [_, white, _],
    !,

    choose_move(GameState, Level, Move),
    move(GameState, Move, NewGameState),
    game_over(NewGameState, Winner),
    valid_moves(NewGameState, NextMoves),
    game_loop([2, BoardSize, Level], NewGameState, Winner, NextMoves).
game_loop([3, BoardSize, Level], GameState, 'DNF', ListOfMoves) :-
    ListOfMoves \= [],
    GameState = [_, white, _],
    !,

    display_game(GameState),

    write('Available pieces:\n'),
    getPiecesFromValidMoves(ListOfMoves, RemainingPieces),
    print_options(RemainingPieces),
    write('Choose a piece to move (e.g., 1-2):\n'),
    valid_input_options(RemainingPieces, SelectedPiece),
    format('You chose: ~w\n', [SelectedPiece]),

    write('Possible moves for this piece:\n'), 
    getMovesFromValidMoves(ListOfMoves, SelectedPiece, MoveOptions),
    print_options(MoveOptions),
    write('Choose a move to that piece (e.g., 1-2):\n'),
    valid_input_options(MoveOptions, SelectedMove),
    format('You chose: ~w\n', [SelectedMove]),

    % Fazer o movimento para essa peça
    move(GameState, (SelectedPiece, SelectedMove), NewGameState),
    game_over(NewGameState, Winner),
    valid_moves(NewGameState, NextMoves),
    game_loop([3, BoardSize, Level], NewGameState, Winner, NextMoves).
game_loop([3, BoardSize, Level], GameState, 'DNF', ListOfMoves) :-
    ListOfMoves \= [],
    GameState = [_, black, _],
    !,

    choose_move(GameState, Level, Move),
    move(GameState, Move, NewGameState),
    game_over(NewGameState, Winner),
    valid_moves(NewGameState, NextMoves),
    game_loop([3, BoardSize, Level], NewGameState, Winner, NextMoves).
game_loop([4, BoardSize, Level], GameState, 'DNF', ListOfMoves) :-
    ListOfMoves \= [],
    !,
    display_game(GameState),
    %sleep(1),
    choose_move(GameState, Level, Move),
    move(GameState, Move, NewGameState),
    game_over(NewGameState, Winner),
    valid_moves(NewGameState, NextMoves),
    game_loop([4, BoardSize, Level], NewGameState, Winner, NextMoves).
game_loop([1, BoardSize, Level], GameState, 'DNF', ListOfMoves):-
    ListOfMoves \= [],
    !,

    display_game(GameState),

    write('Available pieces:\n'),
    getPiecesFromValidMoves(ListOfMoves, RemainingPieces),
    print_options(RemainingPieces),
    write('Choose a piece to move (e.g., 1-2):\n'),
    valid_input_options(RemainingPieces, SelectedPiece),
    format('You chose: ~w\n', [SelectedPiece]),

    write('Possible moves for this piece:\n'), 
    getMovesFromValidMoves(ListOfMoves, SelectedPiece, MoveOptions),
    print_options(MoveOptions),
    write('Choose a move to that piece (e.g., 1-2):\n'),
    valid_input_options(MoveOptions, SelectedMove),
    format('You chose: ~w\n', [SelectedMove]),

    % Fazer o movimento para essa peça
    move(GameState, (SelectedPiece, SelectedMove), NewGameState),
    game_over(NewGameState, Winner),
    valid_moves(NewGameState, NextMoves),
    game_loop([1, BoardSize, Level], NewGameState, Winner, NextMoves).
game_loop([GameMode, BoardSize, Level], [Board, black, ScoreSystem], 'DNF', []) :-
    valid_moves([Board, white, ScoreSystem], WhiteMoves),
    WhiteMoves \= [],
    !,
    game_loop([GameMode, BoardSize, Level], [Board, white, ScoreSystem], 'DNF', WhiteMoves).
game_loop([GameMode, BoardSize, Level], [Board, white, ScoreSystem], 'DNF', []) :-
    valid_moves([Board, black, ScoreSystem], BlackMoves),
    BlackMoves \= [],
    !,
    game_loop([GameMode, BoardSize, Level], [Board, black, ScoreSystem], 'DNF', BlackMoves).
game_loop(_, [Board, _, ScoreSystem], 'draw', _) :-
    display_board(Board),
    write('Game ended with a tie!\n'),
    calculate_score(Board, black, Score, ScoreSystem),
    format('Final Score: ~w\n', [Score]).
game_loop(_, [Board, _, ScoreSystem], Winner, _) :-
    Winner \= 'DNF',
    Winner \= 'draw',
    !,
    display_board(Board),
    format('Game over! Winner: ~w\n', [Winner]),
    calculate_score(Board, Winner, Score, ScoreSystem),
    format('Final Score: ~w\n', [Score]).

% ----------- initial_state(+GameConfig, -GameState)
% Initializes the games initial state based on the provided configuration.
%
% Strategy:
% 1. Extract the BoardSize and ScoreSystem from the GameConfig list, ignoring GameMode and Level.
% 2. Generate the initial game board by calling `generate_board/2` with the extracted BoardSize.
% 3. Set the starting player to black, indicating that the black player makes the first move.
% 4. Combine the generated Board, initial player, and ScoreSystem into the GameState list.
%
% Parameters:
% - GameConfig: A list containing [GameMode, BoardSize, Level, ScoreSystem].
%               (Note: GameMode and Level are not utilized in this predicate.)
% - GameState: A list representing the current state of the game [Board, CurrentPlayer, ScoreSystem].
initial_state([_, BoardSize, _, ScoreSystem], [Board, black, ScoreSystem]) :-
    generate_board(BoardSize, Board).
initial_state([_, BoardSize, _, ScoreSystem], [Board, black, ScoreSystem]) :-
    generate_board(BoardSize, Board).

% ----------- display_game(+GameState)
% Receives game state and displays current player and board
display_game([Board, CurrentPlayer, _]):-
    write('\nCurrent Player: '), write(CurrentPlayer), nl,
    display_board(Board).

% ----------- display_board(+Board)
% Receives the Board and displays board and its coordinates
display_board(Board) :-
    write('Board:\n\n'),
    length(Board, Length),
    show_board(Board, Length),
    write('\n  '),
    print_Coords(1, Length).

% ----------- get_remaining_pieces(+GameState, -RemainingPieces)
% Determines the pieces on the perimeter that match the CurrentPlayer
get_remaining_pieces([Board, CurrentPlayer, _], RemainingPieces) :-
    
    % Take all perimeter pieces
    get_perimeter_positions(Board, Perimeter),

    % Verifies which pieces are from the current user
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
    nth1(Col, BoardRow, Player).

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

% ----------- print_options(+Options)
% Print available pieces to move in the format row-column
print_options([]) :- nl, nl.
print_options([Option|RemainingOptions]) :-
    write(Option),
    write(' '),
    print_options(RemainingOptions).

% ----------- get_piece_moves(+Board, +Position, -Moves)
% Get valid moves for a piece at a given position based on its location and available empty spaces
% In each one, takes row/column, sees how many empties are there and then calculates the possible moves
get_piece_moves(Board, BoardMax, (Row, 1), Moves) :-
    RowIdx is BoardMax - Row + 1,
    nth1(RowIdx, Board, BoardRow),
    count_empty(BoardRow, Count, 0),
    calculate_left_perimeter_moves(Row, Count, Moves).
get_piece_moves(Board, BoardMax, (Row, BoardMax), Moves) :-
    RowIdx is BoardMax - Row + 1,
    nth1(RowIdx, Board, BoardRow),
    count_empty(BoardRow, Count, 0),
    calculate_right_perimeter_moves(Row, BoardMax, Count, Moves).
get_piece_moves(Board, BoardMax, (BoardMax, Col), Moves) :-
    get_column(Board, Col, Column),
    count_empty(Column, Count, 0),
    calculate_top_perimeter_moves(Col, BoardMax, Count, Moves).
get_piece_moves(Board, _, (1, Col), Moves) :-
    get_column(Board, Col, Column),
    count_empty(Column, Count, 0),
    calculate_bottom_perimeter_moves(Col, Count, Moves).

% ----------- count_empty(+Row, -Count, +Acc)
% If 'empty', add it to Accumulator
count_empty([], Acc, Acc).
count_empty([empty|Cells], Count, Acc) :-
    Acc1 is Acc + 1,
    count_empty(Cells, Count, Acc1).
count_empty([_|Cells], Count, Acc):-
    count_empty(Cells, Count, Acc).

% ----------- calculate_X_perimeter_moves(+Row, +Count, -Moves)
% Calculates the possible moves from each side of the perimeter regarding the number of empties in it
calculate_left_perimeter_moves(_, 0, []).
calculate_left_perimeter_moves(Row, Count, [(Row, Col)|Moves]) :-
    Col is Count + 1,
    NewCount is Count - 1, 
    NewCount >= 0,
    calculate_left_perimeter_moves(Row, NewCount, Moves).

calculate_right_perimeter_moves(_, _, 0, []).
calculate_right_perimeter_moves(Row, BoardMax, Count, [(Row, Col)|Moves]) :-
    Col is BoardMax - Count,
    NewCount is Count - 1,
    NewCount >= 0,
    calculate_right_perimeter_moves(Row, BoardMax, NewCount, Moves).

calculate_bottom_perimeter_moves(_, 0, []).
calculate_bottom_perimeter_moves(Col, Count, [(Row, Col)|Moves]) :-
    Row is Count + 1,
    NewCount is Count - 1, 
    NewCount >= 0,
    calculate_bottom_perimeter_moves(Col, NewCount, Moves).

calculate_top_perimeter_moves(_, _, 0, []).
calculate_top_perimeter_moves(Col, BoardMax, Count, [(Row, Col)|Moves]) :-
    Row is BoardMax - Count,
    NewCount is Count - 1,
    NewCount >= 0,
    calculate_top_perimeter_moves(Col, BoardMax, NewCount, Moves).



% ----------- move(+GameState, +SelectedMove, -NewGameState)
% Executes a players move by updating the game board and toggling the current player.
%
% Strategy:
% 1. Identify the Current Player:
%    - Determine the active player from the GameState.
% 2. Execute the Move:
%    - Use the move_pieces/4 predicate to update the game board based on the selected piece and move.
%    - This involves relocating the specified piece to the new position on the board.
% 3. Toggle the Player:
%    - Switch the active player to the other participant (from black to white or vice versa).
% 4. Maintain Score System:
%    - Preserve the current scoring system (ScoreSystem) unchanged in the new game state.
%
% Parameters:
% - GameState: The current state of the game, represented as [Board, CurrentPlayer, ScoreSystem].
%              - Board: The current configuration of the game board.
%              - CurrentPlayer: The player whose turn it is (black or white).
%              - ScoreSystem: The scoring system in use (e.g., standard, product).
% - SelectedMove: A tuple (SelectedPiece, SelectedMove) where:
%                  - SelectedPiece: The identifier of the piece to be moved.
%                  - SelectedMove: The target position to move the piece to.
% - NewGameState: The updated game state after the move, structured as [NewBoard, NextPlayer, ScoreSystem].
%                 - NewBoard: The game board after the selected move has been executed.
%                 - NextPlayer: The player who will take the next turn.
%                 - ScoreSystem: The scoring system remains unchanged.
%
% Clauses:
% - The predicate has two clauses to handle moves for both black and white players.
%   Each clause updates the board accordingly and switches the active player.
move([Board, black, ScoreSystem],  (SelectedPiece, SelectedMove), [NewBoard, white, ScoreSystem]):-
    move_pieces(Board, SelectedPiece, SelectedMove, NewBoard).

move([Board, white, ScoreSystem],  (SelectedPiece, SelectedMove), [NewBoard, black, ScoreSystem]):-
    move_pieces(Board, SelectedPiece, SelectedMove, NewBoard).


% ----------- valid_moves(+GameState, -ListOfMoves)
% Retrieves all valid moves for the current player based on the game state.
%
% Strategy:
% 1. Extract Remaining Pieces
%    - Use get_remaining_pieces/2 to obtain all pieces belonging to the current player that are still available to move.
% 2. Determine Board Size:
%    - Calculate the size of the board with length/2 to understand the boundaries for move calculations.
% 3. Convert Pieces to Options:
%    - Utilize convert_to_options/2 to transform the list of remaining pieces into a format suitable for move generation.
% 4. Generate Valid Moves:
%    - Invoke valid_moves_aux/4 with the board, board size, and remaining pieces to compile a comprehensive list of valid moves (ListOfMoves).
%
% Parameters:
% - GameState: A list representing the current state of the game [Board, CurrentPlayer, ScoreSystem].
%              - Board: The current configuration of the game board.
%              - CurrentPlayer: The player whose turn it is (black or white).
%              - ScoreSystem: The scoring system in use (e.g., standard, product).
% - ListOfMoves: A list of tuples representing all valid moves available to the current player.
%
valid_moves([Board, CurrentPlayer, _], ListOfMoves) :-
    get_remaining_pieces([Board, CurrentPlayer, _], TempRemainingPieces),
    length(Board, BoardSize),
    convert_to_options(TempRemainingPieces, RemainingPieces),
    valid_moves_aux(Board, BoardSize, RemainingPieces, ListOfMoves).
% ----------- valid_moves_aux(+Board, +BoardSize, +RemainingPieces, -ListOfMoves)
% Helper predicate to recursively generate valid moves for each remaining piece.
valid_moves_aux(_, _, [], []).
valid_moves_aux(Board, BoardSize, [Piece|Pieces], [(Piece,Move)|Moves]) :-
    % If there are valid moves, add them to the ListOfMoves.

    % Parse the pieces code to obtain its row and column positions
    parse_move_code(Piece, Row, Col),

    % Retrieve all possible moves for that piece
    get_piece_moves(Board, BoardSize, (Row, Col), TempMove),

    convert_to_options(TempMove, Move),
    Move \= [],
    valid_moves_aux(Board, BoardSize, Pieces, Moves).
valid_moves_aux(Board, BoardSize, [_|Pieces], Moves) :-
    % If a piece has no valid moves, skip it and proceed to the next piece.
    valid_moves_aux(Board, BoardSize, Pieces, Moves).

% ----------- game_over(+GameState, -Winner)
% Checks if the game is over, this means checking if both players are out of valid moves, if so, the winner is calculated with ScoreSystem
game_over(GameState, Winner) :-
    GameState = [Board, _, ScoreSystem],
    valid_moves([Board, white, _], WhiteMoves),
    valid_moves([Board, black, _], BlackMoves),
    game_over_aux(WhiteMoves, BlackMoves, Board, Winner, ScoreSystem).

game_over_aux([], [], Board, Winner, ScoreSystem) :-
    calculate_score(Board, white, ScoreWhite, ScoreSystem),
    calculate_score(Board, black, ScoreBlack, ScoreSystem),
    winner_is(ScoreBlack, ScoreWhite, Winner).
game_over_aux(_, _, _, 'DNF', _).


% ----------- winner_is(+ScoreBlack, +ScoreWhite, -Result)
% Returns the winner after comparing both scores, or a tie
winner_is(ScoreBlack, ScoreWhite, 'black'):- ScoreBlack > ScoreWhite.
winner_is(ScoreBlack, ScoreWhite, 'white'):- ScoreWhite > ScoreBlack.
winner_is(ScoreBlack, ScoreWhite, 'draw'):- ScoreBlack =:= ScoreWhite.


% ----------- calculate_score(+Board, +Player, -Score)
% Calculates score for specified player when game is over, the option 1 is used if user chose the standard score system, 2 for the product score
calculate_score(Board, Player, ScoreStandard, 1):-
    find_player_pieces(Board, Player, ListOfPieces),
    find_groups(ListOfPieces, Groups),
    biggest_group(Groups, ScoreStandard).
calculate_score(Board, Player, ScoreProduct, 2):-
    find_player_pieces(Board, Player, ListOfPieces),
    find_groups(ListOfPieces, Groups),
    multiply_groups(Groups, ScoreProduct). 

% ----------- find_groups(+Pieces, -Groups)
% Finds all groups of adjacent pieces on the board
find_groups(Pieces, Groups) :-
    find_groups_aux(Pieces, [], Groups).

% Accumulates the groups
find_groups_aux([], Groups, Groups).
find_groups_aux([Piece|Rest], Accumulator, Groups) :-
    dfs([Piece], Rest, Group, RemainingPieces),
    append(Accumulator, [Group], NewAccumulator),
    find_groups_aux(RemainingPieces, NewAccumulator, Groups).

% dfs through the pieces to find groups
dfs([], Remaining, [], Remaining).
dfs([Current|Stack], Pieces, [Current|Group], Remaining) :-
    findall(Neighbor, 
            (member(Neighbor, Pieces), adjacent(Current, Neighbor)), 
            Neighbors),
    subtract_our(Pieces, Neighbors, NewPieces),
    append(Neighbors, Stack, NewStack),
    dfs(NewStack, NewPieces, Group, Remaining).


% ----------- adjacent(+Coord1, +Coord2)
% verifies if the marble on coord 1 is orthogonally adjacent to the one on coord 2
adjacent((Row, Col), (R, C)) :-
    (R is Row - 1, C is Col);
    (R is Row + 1, C is Col);
    (R is Row, C is Col - 1);
    (R is Row, C is Col + 1).

% find the group with the largest size -> standard scoring
biggest_group(Groups, Size) :-
    findall(Length, (member(Group, Groups), length(Group, Length)), Lengths),
    max_list_our(Lengths, Size).

% multiply the sizes of all groups -> product scoring
multiply_groups(Groups, Product) :-
    findall(Length, (member(Group, Groups), length(Group, Length)), Lengths),
    foldl_our(multiply, Lengths, 1, Product).

multiply(X, Y, Z) :- Z is X * Y.


% ----------- find_player_pieces(+Board, +Player, -ListOfPieces)
% Finds all coordinates of the specified Players pieces on the Board.
find_player_pieces(Board, Player, ListOfPieces) :-
    findall((Row, Col),
            (nth0(Row, Board, RowList),
             nth0(Col, RowList, Player)),
            ListOfPieces).

% ----------- value(+GameState, +Player, -Value)
% Calculates the heuristic value of a given game state from the perspective of the specified player.
%
% Strategy:
% 1. Calculate Scores:
%    - Use calculate_score/4 to determine the scores for both the computer and the player based on the current board and scoring system.
% 2. Evaluate Heuristic Value:
%    - Compute the heuristic Value by subtracting the computers score from the players score. This value helps in assessing the desirability of the game state for the player.
%
% Parameters:
% - GameState: A list representing the current state of the game [Board, ComputerPlayer, ScoreSystem].
%              - Board: The current configuration of the game board.
%              - ComputerPlayer: The identifier for the computer player (e.g., black or white).
%              - ScoreSystem: The scoring system in use (e.g., standard, product).
% - Player: The player for whom the heuristic value is being calculated (e.g., black or white).
% - Value: The resulting heuristic value representing the advantage of the Player over the computer.
%
% Purpose of Value:
% - Negative Value: Indicates that the Player has an advantage over the computer.
% - Positive Value: Suggests that the computer is in a better position than the Player.
% - Zero Value: Implies a balanced or neutral game state.
value([Board, Computer, ScoreSystem], Player, Value) :-
    calculate_score(Board, Computer, ScorePC, ScoreSystem),
    calculate_score(Board, Player, ScorePlayer, ScoreSystem),
    Value is ScorePC-ScorePlayer.
    
% ----------- choose_move(+GameState, +Level, -BestMove)
% Determines the best move for the computer based on the specified difficulty level.
%
% Strategy:
% 1. **Easy Level (Level = 1):**
%    - Retrieve all valid moves for the computer using valid_moves/2.
%    - Flatten the list of valid moves into a single list of move tuples using flatten_valid_moves/2.
%    - Select a random move from the flattened list using random_member/2.
%
% 2. **Hard Level (Level = 2):**
%    - Retrieve all valid moves for the computer using valid_moves/2.
%    - Flatten the list of valid moves into a single list of move tuples using flatten_valid_moves/2.
%    - Evaluate each move by simulating the move and calculating its heuristic value using evaluate_moves/4.
%    - Select the best move based on the highest heuristic value using select_best_move/2.
choose_move([Board, ComputerPlayer, ScoreSystem], 1, RandMove) :-
    valid_moves([Board, ComputerPlayer, ScoreSystem], ListOfMoves),
    flatten_valid_moves(ListOfMoves, FlatMoves),
    random_member(RandMove, FlatMoves).
choose_move([Board, ComputerPlayer, ScoreSystem], 2, BestMove) :-
    valid_moves([Board, ComputerPlayer, ScoreSystem], ListOfMoves),
    flatten_valid_moves(ListOfMoves, FlatMoves),
    evaluate_moves([Board, ComputerPlayer, ScoreSystem], ComputerPlayer, FlatMoves, EvaluatedMoves),
    write(EvaluatedMoves), nl,
    select_best_move(EvaluatedMoves, BestMove).

% ----------- flatten_valid_moves(+ListOfMoves, -FlatMoves)
% Converts a list of moves structured as [(Piece, [Move1, Move2, ...]), ...] into a flat list of (Piece, Move) tuples.
flatten_valid_moves([], []).
flatten_valid_moves([ (Piece, MoveOptions) | Rest], Moves) :-
    findall((Piece, Move), member(Move, MoveOptions), ExpandedMoves),
    flatten_valid_moves(Rest, RestFlat),
    append(ExpandedMoves, RestFlat, Moves).

% ----------- evaluate_moves(+GameState, +Player, +FlatMoves, -EvaluatedMoves)
% Evaluates each move by simulating it and calculating its heuristic value.
evaluate_moves(_, _, [], []).
evaluate_moves(GameState, Player, [(Piece, Move)|RestMoves], [((Piece, Move), Value)|RestEvaluated]) :-
    move(GameState, (Piece, Move), NewGameState),
    value(NewGameState, Player, Value),
    evaluate_moves(GameState, Player, RestMoves, RestEvaluated).

% ----------- select_best_move(+EvaluatedMoves, -BestMove)
% Selects the best move based on the highest heuristic value.
select_best_move(EvaluatedMoves, BestMove) :-
    findall(Value, member((_, Value), EvaluatedMoves), Values),
    max_list_our(Values, MaxValue),
    findall(Move, member((Move, MaxValue), EvaluatedMoves), BestMoves),
    random_member(BestMove, BestMoves).