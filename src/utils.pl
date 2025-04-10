:- use_module(library(lists)).
:- use_module(library(random)).

% ----------- read_number(+X)
% Reads number input from user and converts it to an integer, X is unified with said integer.
read_number(X) :-
    read_number_aux(0,false,X).


% ----------- read_number_aux(+Acc, +HasReadDigit, -X)
% A helper predicate for read_number/1 that reads input character by character.
read_number_aux(Acc,_,X):-
    get_code(C),
    C >= 48,
    C =< 57,
    !,
    Acc1 is Acc*10 + (C-48),
    read_number_aux(Acc1,true,X).
read_number_aux(X,true,X).

% ----------- valid_input(+Options, -Value)
% Ensures that the users input is a valid option from a given list.
valid_input(Options,Value) :-
    read_number(Value),
    member(Value, Options).

% Repeats unitl a valid input is entered
valid_input(Options, Value) :-
    repeat,
    write('Invalid Input :(\nPlease try again:\n'),
    read_number(Value),
    member(Value,Options),
    !.
% ----------- between(+Min, +Max, -Value)
% Generates values between Min and Max, inclusive.
between(Min, Max, Min):- Min =< Max.
between(Min, Max, Value):-
    Min < Max,
    NewMin is Min + 1,
    between(NewMin, Max, Value).


% ----------- valid_input_options(+Options, -Value)
% Ensures that the users input matches one of the options
valid_input_options(Options, Value) :-
    repeat,
    get_line(Input, []),
    parse_move_code(Input, Row, Col),
    number_codes(Row, RowCodes),
    number_codes(Col, ColCodes),
    append(RowCodes, [45|ColCodes], OptionCodes),
    atom_codes(ParsedAtom, OptionCodes),
    must_be_member(ParsedAtom, Options),
    Value = ParsedAtom,
    !.

% ----------- must_be_member(+X, +List)
% Succeeds if X ∈ List. Otherwise, fails with a retry prompt.
must_be_member(X, List) :-
    member(X, List),
    !.
must_be_member(_, _) :-
    write('\nInvalid Input :(\nPlease try again.\n'),
    fail.

% ----------- parse_move_code(+Code, -Row, -Col)
% Parses the input "Row-Col" => Row=number, Col=number
parse_move_code(Code, NewRow, NewCol) :-
    sub_atom(Code, HyphenIndex, 1, _, '-'), % get index of hyphen
    sub_atom(Code, 0, HyphenIndex, _, RowPart),
    StartColIndex is HyphenIndex + 1,
    sub_atom(Code, StartColIndex, _, 0, ColPart),
    atom_codes(RowPart, RowCodes),
    number_codes(NewRow, RowCodes),
    atom_codes(ColPart, ColCodes),
    number_codes(NewCol, ColCodes).

% ----------- get_line(+Result, +Acc)
% Reads a line of characters until a newline is encountered
get_line(Result, Acc) :-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1), !.
get_line(Result, Acc) :-
    atom_chars(Result, Acc), !.

% ----------- get_column(+Board, +Index, -Column)
% Get a column in a two dimension list
get_column([], _, []).
get_column([Row|Rows], ColIndex, [Elem|Elems]) :-
    nth1(ColIndex, Row, Elem),
    get_column(Rows, ColIndex, Elems).


% ----------- invertList(+List, -InvertedList)
% Inverts a List
invertList(L,Inv):-
    invertList_aux(L,[],Inv).

invertList_aux([],L,L).
invertList_aux([H|T],Acc,Inv):-
    invertList_aux(T,[H|Acc],Inv).

% ----------- convert_to_options(+Tuples, -Options)
% Converts a list of (Row, Column) tuples into a list of Row-Col atoms
convert_to_options([], []).
convert_to_options([(Row, Col)|RestTuples], [Option|RestOptions]) :-
    number_codes(Row, RowCodes),
    number_codes(Col, ColCodes),
    append(RowCodes, [45|ColCodes], OptionCodes),
    atom_codes(Option, OptionCodes),
    convert_to_options(RestTuples, RestOptions).

% Our implementation of subtract
subtract_our([], _, []).
subtract_our([X|Tail], List2, [X|Result]) :-
    \+ member(X, List2),
    subtract_our(Tail, List2, Result).
subtract_our([X|Tail], List2, Result) :-
    member(X, List2),
    subtract_our(Tail, List2, Result).

% Our implementation of max list
max_list_our([X], X).
max_list_our([Head|Tail], Max) :-
    max_list_our(Tail, TailMax),
    Max is max(Head, TailMax).


% ----------- foldl_our(+Pred, +List, +Acc0, -Acc) ----------- 
% Applies Pred to each element of List and the accumulator Acc0.
% Resulting in Acc after processing all elements.
% Our implementation of foldl
foldl_our(_, [], Acc, Acc). 
foldl_our(Pred, [Head|Tail], Acc0, Acc) :-
    call(Pred, Head, Acc0, Acc1),
    foldl_our(Pred, Tail, Acc1, Acc).  

% Our implementation of sublist
% Retrieves sublist from index X to Y (0-based index)
retrieve_sublist(List, X, Y, Sublist) :-
    findall(Elem, (nth1(Index, List, Elem), Index >= X, Index =< Y), Sublist).
