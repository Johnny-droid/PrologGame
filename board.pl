initial_state(GameState):-
    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]].



% ------------------------ GameState Examples ------------------------------

% Test the display of the board
example(1) :-
    GameState = [[0,0,0,0,1,0,0,0,0],
                 [0,0,0,0,0,0,0,0,2],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,1,0,0,0,0,0,2],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,1,0,0]],
    display_game(GameState).


% Test the move input from human player
example(2) :- 
    Player = player(human, 1, 0),

    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    
    display_game(GameState),
    choose_move(GameState, Player, Move),
    write(Move), nl.


% Test the end of the game
example(3) :- 
    Player = player(human, 1, 0),
    NextPlayer = player(human, 2, 0),

    GameState = [[1,0,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0,0],
                 [0,0,1,0,0,0,0,0,0],
                 [0,0,0,1,0,0,0,0,0],
                 [0,0,0,0,2,0,0,0,0],
                 [0,0,0,0,0,1,0,0,0],
                 [0,0,0,0,0,0,1,0,0],
                 [0,0,0,0,0,0,0,1,0],
                 [0,0,0,0,0,0,0,0,1]],

    display_game(GameState),
    game_cycle(GameState, Player-NextPlayer).


% Test the display of the menu
example(4) :- 
    display_main_menu,
    write('Enter your option: '), read(Option),
    write('Your option was: '), write(Option), nl,
    choose_main_menu(Option).
    

% Test the move
example(5) :-
    Player = player(human, 2, 0),

    GameState = [[2,1,1,1,1,1,1,1,1],
                 [1,0,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,0,1,1,1,1,1],
                 [1,1,1,1,0,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1]],
    move(GameState, Player, 1-1, NewGameState),
    display_game(NewGameState).


% Test the valid moves List to make sure every move is included
example(6) :- 
    Player = player(human, 1, 0),

    GameState = [[1,1,1,1,1,1,1,1,1],
                 [1,0,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,0,1,1,1,1,1],
                 [1,1,1,1,2,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1]],
    valid_moves(GameState, Player, Moves),
    write('Moves: '), write(Moves), nl.



example(7) :- 
    Player = player(human, 1, 0),

    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    display_game(GameState),
    check_horizontal(GameState, 1-4, Player, Value),
    write('Value: '), write(Value), nl.


% Test the nthMatrix
example(8) :- 
    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,4,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    nthMatrix(4-4, GameState, Value),
    write('Value: '), write(Value), nl.


example(9) :-
    Player = player(human, 1, 0),

    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,1]],

    check_direction(5-5, 1-1, GameState, Player, Value),
    write('Value: '), write(Value), nl.


example(10) :- 
    Player = player(human, 1, 0),

    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    valid_moves(GameState, Player, Moves),
    write('Moves: '), write(Moves), nl.


% checking if it sees the 2 in the middle that is blocking the 1
example(11) :- 
    Player = player(human, 1, 0),

    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,2,0,0],
                 [0,1,0,0,0,0,2,1,0],
                 [0,0,0,0,0,0,2,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    check_horizontal(GameState, 2-2, Player, Value1),
    write('Value: '), write(Value1), nl.

% checks vertical for 2 in a column
example(12) :- 
    Player = player(human, 2, 0),

    GameState = [[0,0,0,0,1,0,0,0,0],
                 [0,0,0,0,2,0,0,0,0],
                 [0,1,0,0,0,0,0,0,0],
                 [0,0,0,0,0,2,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,1,0,0,0,0],
                 [0,0,0,0,2,0,0,0,0]],
    check_vertical(GameState, 3-4, Player, Value1),
    write('It should be value: 1'), nl,
    write('Value: '), write(Value1), nl.

% test evaluate_row function
example(13) :-
    Player = player(human, 1, 0),

    Row = [0,0,0,1,0,0,0,0,0],
    evaluate_row(Row, 4, 0, Player, 0, Value),
    write('Value: '), write(Value), nl.


% test evaluate_board function
example(14) :-
    Player = player(computer, 1, 2),

    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,1,0,0,0,0,0],
                 [0,0,0,0,1,2,0,0,0],
                 [0,1,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    evaluate_board(GameState, Player, Value),
    write('Value: '), write(Value), nl.




