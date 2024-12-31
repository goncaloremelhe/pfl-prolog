:- use_module(library(lists)).
:-consult('game.pl').
:-consult('utils.pl').
:- consult('board.pl').



play :-
    write('-------------------------------------\n\n'),
    write('Mabula :))\n\n'),
    write('-------------------------------------\n'),
    write('Pick a game mode!!!!\n'),
    write('1. Person vs Person\n'),
    valid_input([1], GameMode),


    write('Pick the number of pieces per side of the board (5-10):\n'),
    valid_input([5, 6, 7, 8, 9, 10], PiecesPerSide),
    BoardSize is PiecesPerSide + 2,

    initial_state([GameMode, BoardSize], GameState),

    display_game(GameState).


