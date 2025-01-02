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
    atom_length(Code, 3),
    sub_atom(Code, 1, 1, 1, '-'),
    sub_atom(Code, 0, 1, _, RowStr),
    sub_atom(Code, 2, 1, _, ColStr),
    atom_chars(RowStr, [RowChar]),
    number_chars(NewRow, [RowChar]),
    atom_chars(ColStr, [ColChar]),
    number_chars(NewCol, [ColChar]).

% ----------- get_line(+Result, +Acc)
% Reads a line of characters until a newline is encountered
get_line(Result, Acc) :-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1), !.
get_line(Result, Acc) :-
    atom_chars(Result, Acc), !.

