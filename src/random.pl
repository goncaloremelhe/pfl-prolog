:- use_module(library(random)).

% Main predicate to generate a random marble list
generate_random_marbles(Half, Result) :-
    findall(MarblesConfig, generate_marbles(Half, Half, [], MarblesConfig), AllSolutions),
    random_member(Marbles, AllSolutions),
    last(Marbles, Last),
    check_wraparound(Marbles, Last, Half, Result).

generate_marbles(0, 0, Acc, Acc).

% Placing a black marble
generate_marbles(BLeft, WLeft, Acc, Marbles) :-
    BLeft > 0,
    can_place(black, Acc),
    BLeft1 is BLeft - 1,
    generate_marbles(BLeft1, WLeft, [black|Acc], Marbles).

% Placing a white marble
generate_marbles(BLeft, WLeft, Acc, Marbles) :-
    WLeft > 0,
    can_place(white, Acc),
    WLeft1 is WLeft - 1,
    generate_marbles(BLeft, WLeft1, [white|Acc], Marbles).

% Check if a marble can be placed (no more than 2 in a row)
can_place(Color, [Color, Color|_]) :- !, fail.
can_place(_, _).

%if first 2 and last element are the same create another result and recursively check if that result wraps around
check_wraparound([FirstElement, FirstElement| _], FirstElement, Half, CheckedResult) :-
    findall(MarblesConfig, generate_marbles(Half, Half, [], MarblesConfig), AllSolutions),
    random_member(Result, AllSolutions),
    last(Result, Last),
    check_wraparound(Result, Last, Half, CheckedResult).

check_wraparound(Marbles, _, _, Marbles).
    
