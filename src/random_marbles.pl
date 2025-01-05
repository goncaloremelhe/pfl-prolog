:- use_module(library(random)).

% Generate a random marble list
generate_random_marbles(Half, Result) :-
    Total is 2 * Half,
    generate_marbles(Half, Half, [], Total, Marbles),
    handle_wraparound(Marbles, Result).

generate_marbles(0, 0, Acc, _, Acc).

% Randomly decide whether to place a black or white marble
generate_marbles(BLeft, WLeft, Acc, Total, Marbles) :-
    BLeft > 0, WLeft > 0,
    random_member(Choice, [0, 1]),
    choose_marble(Choice, BLeft, WLeft, Acc, Total, Marbles).
generate_marbles(BLeft, WLeft, Acc, Total, Marbles) :-
    BLeft > 0,
    try_place(black, BLeft, WLeft, Acc, Total, Marbles).
generate_marbles(BLeft, WLeft, Acc, Total, Marbles) :-
    WLeft > 0,
    try_place(white, BLeft, WLeft, Acc, Total, Marbles).

choose_marble(0, BLeft, WLeft, Acc, Total, Marbles) :-
    try_place(black, BLeft, WLeft, Acc, Total, Marbles).
choose_marble(1, BLeft, WLeft, Acc, Total, Marbles) :-
    try_place(white, BLeft, WLeft, Acc, Total, Marbles).

try_place(Color, BLeft, WLeft, Acc, Total, Marbles) :-
    can_place(Color, Acc),
    update_counts(Color, BLeft, WLeft, BLeft1, WLeft1),
    generate_marbles(BLeft1, WLeft1, [Color|Acc], Total, Marbles).

update_counts(black, BLeft, WLeft, BLeft1, WLeft) :- BLeft1 is BLeft - 1.
update_counts(white, BLeft, WLeft, BLeft, WLeft1) :- WLeft1 is WLeft - 1.

% Check if a marble can be placed (no more than 2 in a row)
can_place(Color, [Color, Color|_]) :- !, fail.
can_place(_, _).

% Handle wrap-around constraints by adjusting the ends
handle_wraparound(Marbles, Result) :-
    reverse(Marbles, [Last|RestReversed]),
    reverse(RestReversed, Rest),
    handle_first_two_elements(Marbles, Last, Rest, Result).

handle_first_two_elements([First, First|_], Last, Rest, Result) :-
    generate_marbles(1, 1, [Last|Rest], _, FixedMarbles),
    Result = [First|FixedMarbles].
handle_first_two_elements(Marbles, _, _, Marbles).
