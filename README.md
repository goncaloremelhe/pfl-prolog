# Mabula - PFL Project

## Indentification:
- Game: Mabula
- Group: 4
- Members:
    - Gonçalo Remelhe, 202205318 (50%) - Implemented....
    - Joana Noites, 202206284 (50%) - Implemented....

## Instalation:
1. Ensure that SICStus is installed.
2. Clone/Download the repository
4. Open Sicstus and use consult comand: consult(your_path/game.pl).
5. Finally type 'play.'

## Description:

Mabula is a connection game that uses two types of marbles (Black and White).
Each of the two players start with 12 (originally) marbles in their color on the perimeter of the playing area. 
Placement of the marbles is randomized as long as no color has more than two marbles in a row, even around a corner.

On a turn, a player chooses a marble on the perimeter of the board, then moves it as many spaces as they wish across the board. 
If other marbles lie in this line, push them ahead of this marble, with the only restriction being that you can't push a marble so far that other marbles are pushed into the perimeter of play. 
Keep taking turns until no player can go. 
Whoever has the largest orthogonally connected group of marbles wins.


## References:

For a more detailed overview of the rulebook and gameplay mechanics, visit the following resources:
- [Mabula Game](https://boardgamegeek.com/boardgame/346743/mabula)

## Considerations for game extensions:

- Visual indication of valid pieces and moves - This helps new users by printing on the game the valid pieces to move as well as their moves.
- Computer Player with two difficulty levels:
    - Easy: The computer randomly selects its pieces and moves.
    - Hard: The computer evaluates the board and selects the best possible move using a greedy/bruteforce (TODO) strategy
- Different Board Sizes:
    - 8x8, 10x10 or 12x12

(TODO)
- Two different scoring systems:
    - Standard : The player with the largest orthogonally connected group of marbles wins.
    - Product: The player with the largest product of the sizes of all their orthogonally connected group of marbles wins.

## Game Logic

### Game Configuration Representation

The game configuration is represented as a list of 3 elements:
- GameMode - Can be represented by numbers 1, 2, 3 or 4. It stores the type of game to be played, 1 meaning Person vs. Person, 2 meaning Person vs. Computer, 3 being Computer vs. Person and 4 being Computer vs. Computer.
- BoardSize - Stores the size of the board, which is calculated depending on the number of marbles the player wants to play with.
- Level - Represents the level of expertise of the Computer, 1 being Easy and 2 being Hard. (TODO) On Level 1, the computer randomly chooses one of the available moves, on Level 2 the Computer chooses its play based on what yields a greater value (greedy strategy)

The GameConfiguration is then passed to the initial_state/2 predicate, which generates the game board based on BoardSize and stores that Board and the GameMode in the first Game State.

(TODO)
Passing the level to the game state when implementing AI, also maybe the score system


### Board Representation

In this game, the internal representation of the board differs from the one displayed to the players, this is done to make the game both more user-friendly for players and easier to manage for developers, as the internal definition of the board is intentionally
made more wordy so the code has better readibility, for example, this is the internal representation of a board in an end-game scenario:

```prolog
Board1 = [
    [blocked, blocked, blocked, blocked, blocked, white,   blocked, blocked],
    [blocked, empty,   empty,   black,   black,   black,   empty,   blocked],
    [blocked, empty,   white,   empty,   white,   white,   black,   blocked],
    [blocked, black,   black,   black,   black,   black,   white,   blocked],
    [blocked, empty,   empty,   white,   white,   black,   white,   blocked],
    [blocked, empty,   empty,   black,   white,   white,   white,   blocked],
    [blocked, empty,   empty,   black,   empty,   white,   empty,   blocked],
    [blocked, blocked, blocked, blocked, blocked, blocked, blocked, blocked]
],
```

In contrast, this is the board displayed to the user, it also includes coordinates and is less wordy and more illustrative:


(meter exemplo da representacao externa) (com pontos em vez de 0s maybe)


### Internal Game State

The game states are represented as lists of 3 elements:
- Board - The current state of the game board, it is a list of lists containing the marbles, the empty spaces and the blocked spaces (places where the marbles can't be moved to).
- CurrPlayer - The player currently playing - black for black marbles or white for white marbles
- GameMode - The type of game to be played, described above.

An initial game state where the black player goes first in a Person vs Person Scenario is represented as:

```prolog
[Board, black, 1]
```

Which shows up like this in the terminal:

(meter)


An intermediate game state where the white player is currently playing in a Person vs Computer Scenario is represented as:

```prolog
[Board, white, 2]
```

Which shows up like this in the terminal:

(meter)

A final game state where the black player is currently playing in a Person vs Person Scenario is represented as:

```prolog
[Board, black, 1]
```

Which shows up like this in the terminal:

(meter)


### Move Representation

As a player, to successfully make a move on the board, first, a marble should be chosen, this is done by entering the coordinates of the marble when prompted in the format X-Y. Since in Mabula each marble can only move in one direction, the next step is to enter the coordinates of the desired destination of the marble in the same format (X-Y), if the move is legal and available, the marble will move to the position, pushing others if they are in its way.

Internally, the move/3 predicate 


### User Interaction

Upon starting the game, a menu is displayed to the user, where they are prompted to choose which game mode they want, then they can also choose the number of marbles they want to play with, which will determine the size of the board, after this, they can start playing.
The menu inputs are validated by checking the user's choice against a predefined list of valid options. Invalid inputs prompt re-entry using a repeat loop until a valid selection is made.

During gameplay, move validation uses the predicate valid_input_options/2. The input is parsed with parse_move_code/3 to extract row and column values in the format Row-Col. These are converted to numbers, reassembled as an atom, and checked against allowed moves (Options). The repeat predicate ensures only valid moves are accepted, looping until the input matches the rules. This guarantees consistency and prevents errors.


## Conclusions

The work carried out allowed for a deeper understanding of the language. Nonetheless, it was a challenging process, made more difficult by the limited time available to familiarize ourselves with the language and the lack of extensive resources.
As for known issues, we acknowledge that we were supposed to let users pick the length and width of the board, but this was too difficult to implement in our game. This complexity arose partly because we also did not implement the feature to place marbles randomly while adhering to specific rules: no more than two marbles in a row (including corners) and ensuring an equal number of marbles for each player. The interplay between these requirements proved challenging to manage, and with the limited time and resources available, we were unable to develop a robust solution for these features.

(podemos muda risto depois lol se conseguirmos fazer) ^ 

## Bibliography
To aid the development of this project, the following resources were consulted:

- [BoardGameGeek Page for the Mabula Game](https://boardgamegeek.com/boardgame/346743/mabula)
- [Steffen Spiele's Official page for Mabula](https://steffen-spiele.com/products/mabula)
- [Official Mabula Rule Book](https://cdn.shopify.com/s/files/1/0760/5141/5360/files/Mabula_All.pdf?v=1694099117)
- [The SWI-Prolog Library](https://eu.swi-prolog.org/pldoc/man?section=libpl)









