read_number(X) :-
    read_number_aux(0,false,X).

read_number_aux(Acc,_,X):-
    get_code(C),
    C >= 48,
    C =< 57,
    !,
    Acc1 is Acc*10 + (C-48),
    read_number_aux(Acc1,true,X).
read_number_aux(X,true,X).


valid_input(Options,Value) :-
    read_number(Value),
    member(Value, Options).

valid_input(Options, Value) :-
    repeat,
    write('Nao foi desta :(\nPodes tentar outra:\n'),
    read_number(Value),
    member(Value,Options),
    !.