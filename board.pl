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
    GameState = [[0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0,0,0]],
    Player = human,
    display_game(GameState-Player),
    choose_move(GameState, Player-1, Move),
    write(Move).


% Test the end of the game
example(3) :- 
    GameState = [[1,0,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0,0],
                 [0,0,1,0,0,0,0,0,0],
                 [0,0,0,1,0,0,0,0,0],
                 [0,0,0,0,2,0,0,0,0],
                 [0,0,0,0,0,1,0,0,0],
                 [0,0,0,0,0,0,1,0,0],
                 [0,0,0,0,0,0,0,1,0],
                 [0,0,0,0,0,0,0,0,1]],
    Player = human,
    display_game(GameState-Player),
    game_over(GameState, Winner),
    congratulate(Winner).


% Test the display of the menu
example(4) :- 
    display_main_menu,
    write('Enter your option: '), read(Option),
    write('Your option was: '), write(Option),
    choose_main_menu(Option).
    

example(5) :-
    GameState = [[1,1,1,1,1,1,1,1,1],
                 [1,0,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,0,1,1,1,1,1],
                 [1,1,1,1,2,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1]],
    move(GameState, 1-1, NewGameState),
    display_game(NewGameState).



example(6) :- 
    GameState = [[1,1,1,1,1,1,1,1,1],
                 [1,0,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,0,1,1,1,1,1],
                 [1,1,1,1,2,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1]],
    valid_moves(GameState, Moves),
    write('Moves: '), write(Moves), nl.