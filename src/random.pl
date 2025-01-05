:- use_module(library(random)).

% Main predicate to generate a random marble list
generate_random_marbles(Half, Marbles) :-
    findall(MarblesConfig, generate_marbles(Half, Half, [], MarblesConfig), AllSolutions),
    random_member(Marbles, AllSolutions).

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

check_wraparound([First, First|_],First) :-
    fail.
    
