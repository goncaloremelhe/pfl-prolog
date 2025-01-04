:- use_module(library(lists)).
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
    valid_input([4, 6, 8, 10], PiecesPerSide),
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
    game_loop(GameConfig, GameState, 'init').


getMovesFromValidMoves([], '').
getMovesFromValidMoves([(Piece,Move)|_], Piece, Move).
getMovesFromValidMoves([_|Moves], Piece, Move) :-
    getMovesFromValidMoves(Moves, Piece, Move).

getPiecesFromValidMoves([], []).
getPiecesFromValidMoves([(Piece, _)|Moves], [Piece|Pieces]):-
    getPiecesFromValidMoves(Moves,Pieces).

% ----------- game_loop(+GameConfig, +GameState)
% starts the game loop with given config

game_loop(_, [Board, _, _], black) :-
    write('Temos um divo!!\n'),
    write('Vencedor: Pretooo\n'),
    write('Score: '),
    calculate_score(Board, black, Score),
    write(Score), nl.
game_loop(_, [Board, _, _], white) :-
    write('Temos um divo!!\n'),
    write('Vencedor: Brancooo\n'),
    write('Score: '),
    calculate_score(Board, white, Score),
    write(Score), nl.
game_loop([1, BoardSize, Level], GameState, _):-
    display_game(GameState),

    valid_moves(GameState, ListOfMoves),
    ListOfMoves \= [],

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
    move(GameState, SelectedPiece, SelectedMove, NewGameState),

    game_over(GameState, Winner),

    game_loop([1, BoardSize, Level], NewGameState, Winner).
game_loop([1, BoardSize, Level], [Board, black, GameMode], 'DNF'):-
    valid_moves([Board, black, GameMode], []), !,
    game_loop([1, BoardSize, Level], [Board, white, GameMode], 'DNF').
game_loop([1, BoardSize, Level], [Board, white, GameMode], 'DNF'):-
    valid_moves([Board, white, GameMode], []), !,
    game_loop([1, BoardSize, Level], [Board, black, GameMode], 'DNF').

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
get_piece_moves([Board, _, _], BoardMax, (Row, 1), Moves) :-
    RowIdx is BoardMax - Row + 1,
    nth1(RowIdx, Board, BoardRow),
    count_empty(BoardRow, Count, 0),
    calculate_left_perimeter_moves(Row, Count, Moves).
get_piece_moves([Board, _, _], BoardMax, (Row, BoardMax), Moves) :-
    RowIdx is BoardMax - Row + 1,
    nth1(RowIdx, Board, BoardRow),
    count_empty(BoardRow, Count, 0),
    calculate_right_perimeter_moves(Row, BoardMax, Count, Moves).
get_piece_moves([Board, _, _], BoardMax, (BoardMax, Col), Moves) :-
    get_column(Board, Col, Column),
    count_empty(Column, Count, 0),
    calculate_top_perimeter_moves(Col, BoardMax, Count, Moves).
get_piece_moves([Board, _, _], _, (1, Col), Moves) :-
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

move([Board, black, _],  SelectedPiece, SelectedMove, [NewBoard, white, _]):-
    move_pieces(Board, SelectedPiece, SelectedMove, NewBoard).

move([Board, white, _],  SelectedPiece, SelectedMove, [NewBoard, black, _]):-
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
    get_piece_moves([Board, _, _], BoardSize, (Row, Col), TempMove),
    convert_to_options(TempMove, Move),
    Move \= [],
    valid_moves_aux(Board, BoardSize, Pieces, Moves).
valid_moves_aux(Board, BoardSize, [_|Pieces], Moves) :-
    valid_moves_aux(Board, BoardSize, Pieces, Moves).

% ----------- game_over(+GameState, -Winner)
% Checks if the game is over, this means checking if both players are out of valid moves, if so, the winner is calculated

game_over([Board, _, _], 'DNF') :-
    valid_moves([Board, white, _], WhiteMoves),
    valid_moves([Board, black, _], BlackMoves),
    WhiteMoves \= [],
    BlackMoves \= [], !.
game_over([Board, _, _], Winner):-
    valid_moves([Board, white, _], WhiteMoves),
    valid_moves([Board, black, _], BlackMoves),
    WhiteMoves = [],
    BlackMoves = [], !,
    calculate_score(Board, white, ScoreWhite),
    calculate_score(Board, black, ScoreBlack),
    winner_is(ScoreBlack, ScoreWhite, Winner),
    write(Winner), nl.

winner_is(ScoreBlack, ScoreWhite, 'black'):- ScoreBlack > ScoreWhite.
winner_is(ScoreBlack, ScoreWhite, 'white'):- ScoreWhite > ScoreBlack.
winner_is(ScoreBlack, ScoreWhite, 'draw'):- ScoreBlack =:= ScoreWhite.


% ------------ calculate_score(+Board, +Player, -Score)
% Calculates score for specified player when game is over, for now returning standard score
calculate_score(Board, Player, ScoreStandard):-
    find_player_pieces(Board, Player, ListOfPieces),
    find_groups(ListOfPieces, Groups),

    biggest_group(Groups, ScoreStandard).
    %multiply_groups(Groups, ScoreProduct). %TODO add to game configuration which scoring the users prefer

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
    foldl(multiply, Lengths, 1, Product).

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


game_over([Board1, _, _], Winner),
write(Winner).


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
