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

