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
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),
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
    write(Move), nl,
    retractall(player(_,_,_)).


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
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),
    display_game(GameState),
    game_cycle(GameState, Player),
    retractall(player(_,_,_)).



% Test the display of the menu
example(4) :- 
    display_main_menu,
    write('Enter your option: '), read(Option),
    write('Your option was: '), write(Option), nl,
    choose_main_menu(Option).
    

% Test the move
example(5) :-
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

    Player = player(human, 2, 0),

    GameState = [[1,1,1,1,1,1,1,1,1],
                 [1,0,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,0,1,1,1,1,1],
                 [1,1,1,1,2,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1],
                 [1,1,1,1,1,1,1,1,1]],
    move(GameState, Player, 1-1, NewGameState),
    display_game(NewGameState),
    retractall(player(_,_,_)).


% Test the valid moves List to make sure every move is included
example(6) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

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
    write('Moves: '), write(Moves), nl,
    retractall(player(_,_,_)).

% Test the next move
example(7) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

    Player = player(human, 1, 0),

    next_player(Player, NextPlayer),

    write('Next player: '), write(NextPlayer), nl,

    retractall(player(_,_,_)).


example(8) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

    Player = player(human, 1, 0),

    RowList = [0,1,0,0,0,0,0,0,0],
    write('RowList: '), write(RowList), nl,
    Value is 0,
    check_horizontal(RowList, 4, Player, Value),
    write('Value: '), write(Value), nl,
    retractall(player(_,_,_)).



example(9) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

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
    write('Value: '), write(Value), nl,
    retractall(player(_,_,_)).


% Test the nthMatrix
example(10) :- 
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


example(11) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

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

    check_direction(6-6, 1-1, GameState, Player, Value),
    write('Value: '), write(Value), nl,
    retractall(player(_,_,_)).


example(12) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

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
    write('Moves: '), write(Moves), nl,
    retractall(player(_,_,_)).


% checking if it sees the 2 in the middle that is blocking the 1
example(13) :- 
    assert(player(human, 1, 0)),
    assert(player(human, 2, 0)),

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
    write('Value: '), write(Value1), nl,
    retractall(player(_,_,_)).


example(14) :- 
    assert(player(human, 1, 0)),
    assert(player(computer, 2, 1)),

    initial_state(GameState),
    player(Type, 1, Level),
    write(Player), nl,
    game_cycle(GameState, player(Type, 1, Level)),

    retractall(player(_,_,_)).