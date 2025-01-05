:- use_module(library(lists)).
:- use_module(library(random)).
:-consult('utils.pl').
:-consult('random_marbles.pl').

% ----------- generate_board(+Size, -Board)
% Generates a board filled entirely with 'empty' cells.
generate_board(Size, Board) :-
    findall(Row, (between(1, Size, _), generate_empty_row(Size, Row)), TempBoard),

    SubListSize is Size - 2,
    MarblesPerColor is 2 * SubListSize,
    generate_random_marbles(MarblesPerColor, MarblesList),

    retrieve_sublist(MarblesList, 1, SubListSize, FirstSubList),
    add_surrounding_elements(corner, FirstSubList, TempLeftColumn),
    invertList(TempLeftColumn, LeftColumn),

    Second is 1 + SubListSize,
    SecondFinal is Second + SubListSize -1,
    retrieve_sublist(MarblesList, Second, SecondFinal, SecondSubList),
    add_surrounding_elements(corner, SecondSubList, BottomRow),

    Third is 1 + SecondFinal,
    ThirdFinal is Third + SubListSize -1,
    retrieve_sublist(MarblesList, Third, ThirdFinal, ThirdSubList),
    add_surrounding_elements(corner, ThirdSubList, RightColumn),

    Fourth is 1 + ThirdFinal,
    FourthFinal is Fourth + SubListSize -1,
    retrieve_sublist(MarblesList, Fourth, FourthFinal, FourthSubList),
    add_surrounding_elements(corner, FourthSubList, TempTopRow),
    invertList(TempTopRow, TopRow),
    
    replace_column(TempBoard, 1, LeftColumn, TempBoard1),
    replace_column(TempBoard1, Size, RightColumn, TempBoard2),
    replace_row(TempBoard2, 1, TopRow, TempBoard3),
    replace_row(TempBoard3, Size, BottomRow, Board).

add_surrounding_elements(Element, List, FinalResult) :-
    Result = [Element | List],  % Add the element at the beginning
    append(Result, [Element], FinalResult).  % Add the element at the end


% ----------- generate_empty_row(+Size, -Row)
% Generates a single row filled with 'empty'.

generate_empty_row(Size, Row) :-
    findall(empty, (between(1, Size, _)), Row).
% ----------- init_cell(+Size, +RowNum, +ColNum, -Piece)
% Determines what to place on a cell depending on x and y coords
init_cell(_,1,1, corner):- !.
init_cell(Size,Size,1, corner):- !.
init_cell(Size,1,Size, corner):- !.
init_cell(Size,Size,Size, corner):- !.

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
piece(empty,'.').
piece(blocked,'X').
piece(placeholder, 'P').
piece(corner, 'C').

% ----------- show_board(+Board)
% writes Board on the console, row by row
show_board([], _).
show_board([Row|RemainingRows], Coord) :-
    coordinate_spacing(Coord),
    NewCoord is Coord-1,
    show_row(Row), nl,
    show_board(RemainingRows, NewCoord).

coordinate_spacing(Coord):-
    Coord < 10,
    write(Coord), write('  ').
coordinate_spacing(Coord):-
    Coord >= 10,
    write(Coord), write(' ').


% ----------- show_row(+Row)
% shows a row of the board on the console, piece by piece
show_row([]).
show_row([Piece|RemainingPieces]) :-
    show_piece(Piece),
    write(' '),
    show_row(RemainingPieces).

% ----------- show_piece(+Piece)
% Displays a piece based on its type
show_piece(Piece) :-
    piece(Piece, 'C'),
    write(' ').
show_piece(Piece) :-
    piece(Piece, P),
    write(P).
show_piece(blocked) :-
    write('X').
show_piece(_) :-
    write(' ').

%Left/Right Move
move_pieces(Board, SelectedPiece, SelectedMove, NewBoard) :-
    parse_move_code(SelectedPiece, RowPiece, ColPiece),
    parse_move_code(SelectedMove, RowMove, ColMove),
    RowPiece = RowMove,
    length(Board, TotalRows),
    BoardRowIndex is TotalRows - RowPiece + 1,

    % Row antes de mudar
    nth1(BoardRowIndex, Board, OldRow),

    % Obter Peça
    nth1(ColPiece, OldRow, Piece),

    % Mover Peça
    move_piece_recursively(OldRow, ColPiece, ColMove, Piece, TempRow),

    % Mudar para Blocked
    replace_element(TempRow, ColPiece, blocked, NewRow),

    % Mudar Row
    replace_row(Board, BoardRowIndex, NewRow, NewBoard).

% Up/Down Move
move_pieces(Board, SelectedPiece, SelectedMove, NewBoard) :-
    parse_move_code(SelectedPiece, RowPiece, ColPiece),
    parse_move_code(SelectedMove, RowMove, ColMove),
    ColPiece = ColMove,

    % Get Column
    get_column(Board, ColPiece, Column),
    invertList(Column, OldColumn),

    % Obter Peça
    nth1(RowPiece, OldColumn, Piece),


    % Mover Peça
    move_piece_recursively(OldColumn, RowPiece, RowMove, Piece, TempRow),

    % Mudar para Blocked
    replace_element(TempRow, RowPiece, blocked, NewRow),

    % Mudar Column
    replace_column(Board, ColPiece, NewRow, NewBoard).



move_piece_recursively(Row, TargetCol, TargetCol, _, Row).

move_piece_recursively(Row, CurrentCol, TargetCol, Piece, NewRow) :-
    % Direçao Esquerda vai dar -1, Direita vai dar 1
    Direction is sign(TargetCol - CurrentCol),

    % Proxima Coluna
    NextCol is CurrentCol + Direction,

    % Se for vazia, apenas avança
    nth1(NextCol, Row, empty),

    replace_element(Row, CurrentCol, empty, TempRow1),
    replace_element(TempRow1, NextCol, Piece, TempRow2),

    move_piece_recursively(TempRow2, NextCol, TargetCol, Piece, NewRow).

move_piece_recursively(Row, CurrentCol, TargetCol, Piece, NewRow) :-
    % Direçao Esquerda vai dar -1, Direita vai dar 1
    Direction is sign(TargetCol - CurrentCol),

    % Proxima Coluna
    NextCol is CurrentCol + Direction,

    % Caso NextPiece nao seja vazia, empurrar
    nth1(NextCol, Row, NextPiece),
    NextPiece \= empty,

    Dest is CurrentCol + Direction + Direction,

    % Move Primeiro a NextPiece
    move_piece_recursively(Row, NextCol, Dest, NextPiece, TempRow1),
    
    % Agora altera a Piece
    replace_element(TempRow1, CurrentCol, empty, TempRow2),
    replace_element(TempRow2, NextCol, Piece, TempRow3),

    move_piece_recursively(TempRow3, NextCol, TargetCol, Piece, NewRow).


replace_row(Board, RowIndex, NewRow, NewBoard) :-
    nth1(RowIndex, Board, _, Rest),
    nth1(RowIndex, NewBoard, NewRow, Rest).

replace_element(List, Index, Element, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Element, Rest).


replace_column(Board, ColumnIndex, NewRow, NewBoard) :-
    invertList(NewRow, ReversedColumn),
    replace_column_helper(Board, ColumnIndex, ReversedColumn, NewBoard).

replace_column_helper([], _, [], []).
replace_column_helper([Row|RestRows], ColIndex, [NewElement|RestElements], [NewRow|NewBoard]) :-
    replace_element(Row, ColIndex, NewElement, NewRow),
    replace_column_helper(RestRows, ColIndex, RestElements, NewBoard).