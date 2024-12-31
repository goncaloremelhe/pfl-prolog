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
    write('Nao foi desta :(\nPodes tentar outra:\n'),
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