# PrologGame
Game: Center  
Group: Center_3   

João Ricardo Ramos Alves (up202007614) - 60%  
Pedro Miguel Magalhães Nunes (up202004714) - 40%



## Installation and execution  

The only software required to run this project is SICStus Prolog 4.7.1.

```
sicstus -l proj.pl
```

Run the predicate play/0 to enter the main menu and start playing.  

```
?- play.
```


## Game description  

- Center can be played in a 9x9 board (or similar - chess board(8x8) has 9x9 intersections, therefore can be used).    
- Players take turns placing one piece at a time.  
- A piece placed N steps away from the perimeter must have at least N friendy pieces in sight.  
- On a square board, pieces see in 8 directions.  
- The winner is the player who places a piece on the center of the board.  

## Game Logic  

### Internal representation of the state of the game  

The game state is composed of the current state of the board and the information of player's 1 and 2.  

- The board is represented as a list of lists (9x9). Each list represents a row on the board, and each list element represents a board cell. A cell is represented as an atom, indicating if there is a piece placed, and which player has placed it (0 - empty cell; 1 - placed by Player 1; 2 - placed by Player 2).  

- The players are represented by a predicate **player/3**, which carries information on the player's type (human or computer), number (1 or 2) and level of difficulty (0 for human, 1, 2, 3 or 4 for computer).  

`Initial State`  

```
player(computer, 1, 3),
player(computer, 2, 1).
```  

```
[[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0]].
```  

![Initial State](img/initial_state.png)  

`Intermediate State`  

```
player(computer, 1, 3),
player(computer, 2, 1).
```  

```
[[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[1,0,0,0,2,0,0,0,0]].
```  

![Intermediate State](img/intermediate_state.png)  

`Final State`  

```
player(computer, 1, 3),
player(computer, 2, 1).
```  

```
[[1,0,0,0,0,0,0,0,1],
[0,0,0,0,2,2,2,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[1,0,0,0,1,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0],
[1,0,0,0,2,0,0,0,0]].
```  

![Final State](img/final_state.png)  

### Game state view  

Regarding the display of the game, there are two focal points: displaying the menus and displaying the game being played.  

- As for the menus, the predicate **display_main_menu/0** is used to display a header for the selection menus, and **choose_player/2** **choose_level/2** display the selection menus to configure the game.  

![Menus](img/menus.png)

- The state of the board is shown using **display_game/1**. This predicate makes use of various auxiliary predicates to present a clear and understandable board to the player (**display_header/0**, **display_game_aux/2**, **display_row/1**, **display_cell/1**). In the game board, the pieces placed by player 1 are presented as a 'X' and the ones placed by player 2 by an 'O'.

- Finally, regarding the players' information, the player currently placing a piece in the board is indicated by **choose_move/3** and the winning player is indicated by **congratulate/1**.  

![Playing](img/playing.png)  

### Moves Execution  

In order to check to validate and apply a move we created the predicate **move/4**:

```
move(+GameState, +Player, +Move, -NewGameState)
```  

The predicate will fail if the given move is not valid.  

There is only one type of move, which is validated based on the previous moves from both players (the current state of the board) as well as the obvious boundaries of the board.  

To validate a move we created a predicate **valid_move/3** which enforces the game logic. This predicate makes use of multiple auxiliary predicates to help us check how many "friendly pieces in sight" there are in all 8 directions from a certain position: **check_all_directions/4**, **check_direction/5**,**check_direction_aux/5**, **check_horizontal/4**, **check_vertical/4**, **check_diagonal1/4** and **check_diagonal2/4**. There is as well as a predicate to calculate the distance of any position to the border of the board: **border_distance/2**.  

Finally, the predicate **replace/4** is used to alter the game state.  


### List of Valid Moves  

An important predicate to allow the game to be played by the computer is **valid_moves/3** which gives us a list of every valid move.  

```
valid_moves(+GameState, +Player, -Moves)
```  

Using **findall/3** and **move/4** we are able to find all valid moves a player can make on a given turn.

### End of Game  

In order to check if the game ended we created the predicate **game_over/2**.  

```
game_over(+GameState, -Winner)
```  

There is only one way to win the game, that is to place a piece in the very center of the board (4, 4). So we simply check if the value for that position is different than 0 (no piece) and if so then that value corresponds to the winner (1 or 2).  

### Board Evaluation  

In order to evaluate the state of the board we used a predicate **evaluate_board/3**.  

```
evaluate_board(+GameState, +Player, -Value)
```  

This predicate has three different behaviours. This approach was used to allow more levels of difficulty in the game.  

The first one uses the auxiliary predicates **evaluate_board_aux/5** and **evaluate_row/6** in order to find the highest distance from the border of the board where the current player can place a piece. This approach will be used for Level 2.  

The second one is to use **check_all_directions/4** from the center of the board, with the value being the number of "friendly pieces in sight" from the center. This approach will be used for Level 3.  

The last one is similar, in addiction to checking for the current player from the center, we also use the predicate **check_all_directions/4** on the opposing player also from the center of the board. Then the value will be a subtraction of the number of pieces in sight of the current player and the number of pieces in sight of the other player. This approach will be used for Level 4.  

### Computer move  

To allow the computer choosing a move the predicate **choose_move/3** as different behaviour for computer players, which relies on the predicate **choose_move_computer/4**.  

```
choose_move(+GameState, +Player, -Move)

choose_move_computer(+GameState, +Player, +Moves, -Move)
```  

The second one receiving a list of every possible move, and selecting one based on different criteria depending on the level of difficulty.  

- Level 1 (Easy): Chooses a random move, using random_select/3.  
- Level 2 (Medium): Chooses the valid move with a higher distance to the border (closer to the center). The goal is to reach a cell with distance 4 (which can only be the center).  
- Level 3 (Hard): Chooses a move that will maximise the number of pieces in sight from the center, making it so that a piece can be placed in center as fast as possible.  
- Level 4 (Very Hard): Chooses a move that will maximise the difference of the number of pieces in sight from the center for the current player and the number of pieces in sight from the center for the opponent. The goal is to simultaneously try to place a piece in the center as fast as possible while making it harder for the opponent to do the same.  

## Conclusions  

The board game *Center* was successfully implemented in the SicStus Prolog 4.7.1 language. The game can be played Player vs Player, Player vs Computer or Computer vs Computer (with the same or different levels).

## Bibliography  

https://boardgamegeek.com/boardgame/360905/center  
https://sicstus.sics.se/sicstus/docs/4.7.1/html/sicstus/