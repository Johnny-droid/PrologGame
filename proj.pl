% GameState is a list of lists (9x9)

play_game:-
    initial_state(GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player).

game_cycle(GameState-Player):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player):-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(GameState-NextPlayer), !,
    game_cycle(NewGameState-NextPlayer).



choose_move(GameState, human, Move):- !. % change this
    % interaction to select move

choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).

valid_moves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).
    choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).

choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
        move(GameState, Mv, NewState),
        evaluate_board(NewState, Value) ), [_V-Move|_]).





% ------------------------- DISPLAY GAME --------------------------------

display_game(GameState-Player) :-
    display_header(),
    display_game_aux(GameState-Player, 1).

display_header() :-
    write('    -------------------------------------'), nl,
    write('    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |'), nl,
    write('-----------------------------------------'), nl.   

display_game_aux([]-_, _) :- !.
display_game_aux([Row | Tail]-Player, N) :-
    format('| ~d | ', [N]),
    display_row(Row), nl,
    write('-----------------------------------------'), nl,
    N1 is N+1,
    display_game_aux(Tail-Player, N1).

display_row([]) :- !.
display_row([H | T]) :- 
    display_cell(H), write(' | '),
    display_row(T).

display_cell(H) :-
    (H = 0 -> write('.');
    H = 1 -> write('X');
    H = 2 -> write('O')).













% ------------------------ GameState Examples ------------------------------


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
    Player = human,
    display_game(GameState-Player).

