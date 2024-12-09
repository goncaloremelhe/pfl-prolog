:-consult('game.pl').
:-consult('utils.pl').


play :-
    write('-------------------------------------\n\n'),
    write('Mabula :))\n\n'),
    write('-------------------------------------\n'),
    write('Escolhe um modo de jogo!!!!\n'),
    write('1. Pessoa vs Pessoa\n'),
    valid_input([1], Input),
    game(Input).