

% ----------- generate_board(+Size, -Board)
% Generates the board in its initial state, a square of size Size
generate_board(Size, Board) :-
    findall(Row, (between(1, Size, RowNum), generate_row(Size, RowNum, Row)), Board).

% ----------- generate_row(+Size, +RowNum, -Row)
% Generates a row of the board on its initial state
generate_row(Size, RowNum, Row) :-
    findall(Cell, (between(1, Size, ColNum), init_cell(Size, RowNum, ColNum, Cell)), Row).

% ----------- init_cell(+Size, +RowNum, +ColNum, -Piece)
% Determines what to place on a cell depending on x and y coords
init_cell(_, 1, 1, blocked):- !.
init_cell(Size, Size, 1, blocked):- !.
init_cell(Size, 1, Size, blocked):- !.
init_cell(Size, Size, Size, blocked):- !.

% Alternating black and white marbles on the border
init_cell(_, 1, ColNum, Piece) :- alternating_marble(1, ColNum, Piece), !.
init_cell(Size, Size, ColNum, Piece) :- alternating_marble(Size, ColNum, Piece), !.
init_cell(_, RowNum, 1, Piece) :- alternating_marble(RowNum, 1, Piece), !.
init_cell(Size, RowNum, Size, Piece) :- alternating_marble(RowNum, Size, Piece), !.

% Empty cells in the center of the board
init_cell(_, _, _, empty):- !.

% ----------- alternating_marble(+Row, +Col, -Piece)
% Determines whether to place black or white based on the row and column indices
alternating_marble(Row, Col, black) :- (Row + Col) mod 2 =:= 0, !.
alternating_marble(_, _, white).


% ----------- piece(+Name, -Char)
% returns the char corresponding to piece with name Name
piece(black,'B').
piece(white,'W').
piece(empty,'0').
piece(blocked,'X').
piece(placeholder, 'P').

% ----------- show_board(+Board)
% writes Board on the console, row by row
show_board([]).
show_board([Row|RemainingRows]) :-
    show_row(Row), nl,
    show_board(RemainingRows).


% ----------- show_row(+Row)
% shows a row of the board on the console, piece by piece
show_row([]).
show_row([Piece|RemainingPieces]) :-
    show_piece(Piece),
    write(' '),
    show_row(RemainingPieces).

% ----------- show_piece(+Piece)
% shows a piece
show_piece(Piece):-
    piece(Piece, P),
    write(P).
show_piece(_) :-
    write(' ').

