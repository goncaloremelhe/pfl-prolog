

% ----------- generate_board(+Size, -Board)
% Generates the board in its initial state, a square of size Size
generate_board(Size, Board) :-
    findall(Row, (between(1, Size, RowNum), generate_row(Size, RowNum, Row)), Board).

% ----------- generate_row(+Size, +RowNum, -Row)
% Generates a row of the board on its initial state
generate_row(Size, RowNum, Row) :-
    findall(Cell, (between(1, Size, ColNum), init_cell(Size, RowNum, ColNum, Cell)), Row).

% ----------- init_cell(+Size,+RowNum,+ColNum,-Piece)
% determines what to place on a cell depending on x and y coords
% cells at the borders are blocked

init_cell(_,1,1, blocked):- !.
init_cell(Size,Size,1, blocked):- !.
init_cell(Size,1,Size, blocked):- !.
init_cell(Size,Size,Size, blocked):- !.

% placeholder should be replaced by black or white randomly (TODO)

init_cell(_, 1, _, placeholder):- !.
init_cell(Size, Size, _, placeholder):- !.
init_cell(_, _, 1, placeholder):- !.
init_cell(Size, _, Size, placeholder):- !.
init_cell(_, _, _, empty):- !. 


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

