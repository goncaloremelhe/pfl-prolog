:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('utils.pl').
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
    valid_input([2,4, 6, 8, 10], PiecesPerSide),
    BoardSize is PiecesPerSide + 2,
    
    write('\nPick the scoring system (standard or product):\n'),
    write('1. Standard\n'),
    write('2. Product\n'),
    valid_input([1,2], ScoreSystem),
    game([GameMode, BoardSize, Level, ScoreSystem]).


% Por agora nao faz nada
difficultyLevel(1,0).
difficultyLevel(GameMode, Level):-
    GameMode > 1,
    write('\nChoose the level difficulty:\n'),
    write('1. Easy\n'),
    write('2. Hard\n'),
    valid_input([1,2], Level).


game([GameMode, BoardSize, Level, ScoreSystem]):-
    initial_state([GameMode, BoardSize, Level, ScoreSystem], GameState),
    valid_moves(GameState, ListOfMoves),
    game_loop([GameMode, BoardSize, Level], GameState, 'DNF', ListOfMoves).


getMovesFromValidMoves([], '').
getMovesFromValidMoves([(Piece,Move)|_], Piece, Move).
getMovesFromValidMoves([_|Moves], Piece, Move) :-
    getMovesFromValidMoves(Moves, Piece, Move).

getPiecesFromValidMoves([], []).
getPiecesFromValidMoves([(Piece, _)|Moves], [Piece|Pieces]):-
    getPiecesFromValidMoves(Moves,Pieces).

% ----------- game_loop(+GameConfig, +GameState)
% starts the game loop with given config

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

    write(GameState), nl,
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

% ----------- initial_state(+GameConfig)
% Receives game configuration and returns the initial game state (player with the black pieces is starting for now but we can change this later)
initial_state([_, BoardSize, _, ScoreSystem], [Board, black, ScoreSystem]) :-
    generate_board(BoardSize, Board).

% ----------- display_game(+GameState)
% Receives game state and displays current player and board
display_game([Board, CurrentPlayer, _]):-
    write('\nCurrent Player: '), write(CurrentPlayer), nl,
    display_board(Board).

display_board(Board) :-
    write('Board:\n\n'),
    length(Board, Length),
    show_board(Board, Length),
    write('\n  '),
    print_Coords(1, Length).

% ----------- get_remaining_pieces(+GameState, -RemainingPieces)
% Determines the pieces on the perimeter that match the CurrentPlayer
get_remaining_pieces([Board, CurrentPlayer, _], RemainingPieces) :-
    
    % Pega em todas as peças do perimetro
    get_perimeter_positions(Board, Perimeter),

    %Verifica quais dão match ao jogador atual
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
% Em cada uma, pega na row/column, vê quantos empty tem e calcula os mvoes possiveis
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

% Caso seja empty, adiciona ao Acc, caso contrario, passa para a proxima
count_empty([], Acc, Acc).
count_empty([empty|Cells], Count, Acc) :-
    Acc1 is Acc + 1,
    count_empty(Cells, Count, Acc1).
count_empty([_|Cells], Count, Acc):-
    count_empty(Cells, Count, Acc).

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

move([Board, black, ScoreSystem],  (SelectedPiece, SelectedMove), [NewBoard, white, ScoreSystem]):-
    move_pieces(Board, SelectedPiece, SelectedMove, NewBoard).

move([Board, white, ScoreSystem],  (SelectedPiece, SelectedMove), [NewBoard, black, ScoreSystem]):-
    move_pieces(Board, SelectedPiece, SelectedMove, NewBoard).


% ----------- valid_moves(+GameState, -ListOfMoves)
% Retrieves all valid moves for the current player
valid_moves([Board, CurrentPlayer, _], ListOfMoves) :-
    get_remaining_pieces([Board, CurrentPlayer, _], TempRemainingPieces),
    length(Board, BoardSize),
    convert_to_options(TempRemainingPieces, RemainingPieces),
    valid_moves_aux(Board, BoardSize, RemainingPieces, ListOfMoves).

valid_moves_aux(_, _, [], []).
valid_moves_aux(Board, BoardSize, [Piece|Pieces], [(Piece,Move)|Moves]) :-
    parse_move_code(Piece, Row, Col),
    get_piece_moves(Board, BoardSize, (Row, Col), TempMove),
    convert_to_options(TempMove, Move),
    Move \= [],
    valid_moves_aux(Board, BoardSize, Pieces, Moves).
valid_moves_aux(Board, BoardSize, [_|Pieces], Moves) :-
    valid_moves_aux(Board, BoardSize, Pieces, Moves).

% ----------- game_over(+GameState, -Winner)
% Checks if the game is over, this means checking if both players are out of valid moves, if so, the winner is calculated
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



winner_is(ScoreBlack, ScoreWhite, 'black'):- ScoreBlack > ScoreWhite.
winner_is(ScoreBlack, ScoreWhite, 'white'):- ScoreWhite > ScoreBlack.
winner_is(ScoreBlack, ScoreWhite, 'draw'):- ScoreBlack =:= ScoreWhite.


% ------------ calculate_score(+Board, +Player, -Score)
% Calculates score for specified player when game is over, for now returning standard score
calculate_score(Board, Player, ScoreStandard, 1):-
    find_player_pieces(Board, Player, ListOfPieces),
    find_groups(ListOfPieces, Groups),
    biggest_group(Groups, ScoreStandard).
calculate_score(Board, Player, ScoreProduct, 2):-
    find_player_pieces(Board, Player, ListOfPieces),
    find_groups(ListOfPieces, Groups),
    multiply_groups(Groups, ScoreProduct). 

% Main predicate: Finds all groups of adjacent pieces
find_groups(Pieces, Groups) :-
    find_groups_aux(Pieces, [], Groups). % Start the recursive search with an empty accumulator.

% Base case: no more pieces, no more groups.
find_groups_aux([], Groups, Groups).

% Recursive case: process a piece, find its group, and continue with the rest.
find_groups_aux([Piece|Rest], Accumulator, Groups) :-
    dfs([Piece], Rest, Group, RemainingPieces), % Find one group starting with Piece.
    append(Accumulator, [Group], NewAccumulator), % Add the found group to the accumulator.
    find_groups_aux(RemainingPieces, NewAccumulator, Groups). % Continue with the remaining pieces.

% DFS to find all connected pieces
dfs([], Remaining, [], Remaining). % Base case: no more pieces to explore.
dfs([Current|Stack], Pieces, [Current|Group], Remaining) :-
    findall(Neighbor, 
            (member(Neighbor, Pieces), adjacent(Current, Neighbor)), 
            Neighbors), % Find all unvisited neighbors.
    subtract_our(Pieces, Neighbors, NewPieces), % Remove Neighbors from Pieces.
    append(Neighbors, Stack, NewStack), % Add Neighbors to the stack.
    dfs(NewStack, NewPieces, Group, Remaining). % Continue DFS.



% Check for orthogonal adjacency
adjacent((Row, Col), (R, C)) :-
    (R is Row - 1, C is Col);
    (R is Row + 1, C is Col);
    (R is Row, C is Col - 1);
    (R is Row, C is Col + 1).

% find the group with the largest size -> standard scoring
biggest_group(Groups, Size) :-
    findall(Length, (member(Group, Groups), length(Group, Length)), Lengths),
    max_list_our(Lengths, Size).

% Multiply the sizes of all groups -> product scoring
multiply_groups(Groups, Product) :-
    findall(Length, (member(Group, Groups), length(Group, Length)), Lengths),
    foldl_our(multiply, Lengths, 1, Product).

multiply(X, Y, Z) :- Z is X * Y.


%-------------- find_player_pieces(+Board, +Player, -ListOfPieces)
% Finds all coordinates of the specified Players pieces on the Board.
find_player_pieces(Board, Player, ListOfPieces) :-
    findall((Row, Col),
            (nth0(Row, Board, RowList),
             nth0(Col, RowList, Player)),
            ListOfPieces).



test_game_over:-  
Board1 = [
    [blocked, blocked, blocked, blocked, blocked, white,   blocked, blocked],
    [blocked, empty,   empty,   black,   black,   black,   empty,   blocked],
    [blocked, empty,   white,   empty,   white,   white,   black,   blocked],
    [blocked, black,   black,   black,   black,   black,   white,   blocked],
    [blocked, empty,   empty,   white,   white,   black,   white,   blocked],
    [blocked, empty,   empty,   black,   white,   white,   white,   blocked],
    [blocked, empty,   empty,   black,   empty,   white,   empty,   blocked],
    [blocked, blocked, blocked, blocked, blocked, blocked, blocked, blocked]
],
valid_moves([Board1, white, _], P),
write(P).


test_game_over1:-  
Board1 = [
    [blocked, blocked, blocked, blocked, blocked, white,   blocked, blocked],
    [black, empty,   empty,   black,   black,   empty,   empty,   blocked],
    [blocked, empty,   white,   empty,   white,   white,   black,   blocked],
    [blocked, black,   black,   black,   black,   black,   white,   blocked],
    [white, empty,   empty,   white,   white,   black,   white,   blocked],
    [blocked, empty,   empty,   black,   white,   white,   white,   blocked],
    [blocked, empty,   empty,   black,   empty,   white,   empty,   blocked],
    [blocked, blocked, blocked, blocked, blocked, blocked, blocked, blocked]
],
valid_moves([Board1, white, _], P),
valid_moves([Board1, black, _], P1),
write(P),
nl, write(P1).


value([Board, Computer, ScoreSystem], Player, Value) :-
    calculate_score(Board, Computer, ScorePC, ScoreSystem),
    calculate_score(Board, Player, ScorePlayer, ScoreSystem),
    Value is ScorePlayer-ScorePC.
    
% ----------- choose_move(+GameState, +Level, -BestMove) ----------- 
% Determines the best move for the Computer based on the specified Level.
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

flatten_valid_moves([], []).
flatten_valid_moves([ (Piece, MoveOptions) | Rest], Moves) :-
    findall((Piece, Move), member(Move, MoveOptions), ExpandedMoves),
    flatten_valid_moves(Rest, RestFlat),
    append(ExpandedMoves, RestFlat, Moves).

evaluate_moves(_, _, [], []).
evaluate_moves(GameState, Player, [(Piece, Move)|RestMoves], [((Piece, Move), Value)|RestEvaluated]) :-
    move(GameState, (Piece, Move), NewGameState),
    value(NewGameState, Player, Value),
    evaluate_moves(GameState, Player, RestMoves, RestEvaluated).

select_best_move(EvaluatedMoves, BestMove) :-
    findall(Value, member((_, Value), EvaluatedMoves), Values),
    max_list_our(Values, MaxValue),
    findall(Move, member((Move, MaxValue), EvaluatedMoves), BestMoves),
    random_member(BestMove, BestMoves).