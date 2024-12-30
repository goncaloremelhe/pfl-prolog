game(Input) :-
    write('ola').


initial_state(0, _, _, []).


/*

board([
    [X, X, X, X, X, X, X, X],
    [X, 0, black , white , 0, 0, 0, black ],
    [X, white , 0, black , black , 0, 0, white ],
    [X, 0, black , 0, white , 0, black , 0],
    [black , 0,  white , white , 0, black, 0, X],
    [X, 0, 0, black , 0, 0, white , X],
    [X,  white , 0, 0,  black ,  white , 0,  black],
    [X,  white , 0, 0,  black ,  white , 0,  black],
    [X, X, X, X, X, X, X, X]
]).
*/
