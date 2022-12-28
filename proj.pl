:- consult('board.pl').

% GameState is a list of lists (9x9)

% ------------------------- GAME MAIN STRUCTURE --------------------------------
play :-
    display_main_menu,
    write('Enter your option: '), read(Option),
    choose_main_menu(Option).

play_game():-
    initial_state(GameState),
    display_game(GameState),
    Player = player(_, 1, _),
    write(Player), nl,
    game_cycle(GameState, Player).

game_cycle(GameState, Player):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState, Player):-
    choose_move(GameState, Player, Move),
    move(GameState, Player, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState), !,
    game_cycle(NewGameState, NextPlayer).



% ------------------------- GAME LOGIC --------------------------------
choose_move(GameState, player(human, X, _), Move):- % still needs to check if move is valid
    format('Player ~d turn!', [X]), nl,
    write('Enter row: '), read(Row),
    write('Enter column: '), read(Column),
    Move = Row-Column.

choose_move(GameState, player(computer,X,Level), Move):-
    valid_moves(GameState, Player, Moves),
    choose_move_computer(Level, GameState, Moves, Move).

valid_moves(GameState, Player, Moves):-
    findall(
        Row-Column, 
        (between(0,8,Row),
        between(0,8,Column),
        move(GameState, Player, Row-Column, NewState)),
        Moves).

choose_move_computer(1, _GameState, Player, Moves, Move):-
    random_select(Move, Moves, _Rest).

choose_move_computer(2, GameState, Player, Moves, Move):-
    setof(Value-Mv, 
        NewState^( member(Mv, Moves),
        move(GameState, Mv, Player, NewState),
        evaluate_board(NewState, Value) ), [_V-Move|_]).


move(GameState, player(_, N, _), Row-Column, NewGameState):- % needs to validate move
    nth(Row, GameState, RowList),
    nth(Column, RowList, 0),
    replace(Column, RowList, N, NewRowList),
    replace(Row, GameState, NewRowList, NewGameState).

next_player(player(_, 1, _), NextPlayer) :- 
    NextPlayer = player(X, 2, Y), !.

next_player(player(_, 2, _), NextPlayer) :-
    NextPlayer = player(_, 1, _), !.


game_over(GameState, Winner) :- 
    nth(4, GameState, Row),
    nth(4, Row, Winner), (Winner \== 0), !.




% ------------------------- GAME MENUS --------------------------------

display_main_menu :- 
    write(' ------------------------------------------------- \n'),
    write('|                                                 | \n'),  
    write('|                     CENTER                      | \n'),
    write('|                                                 | \n'),   
    write('|              1. Player vs Player                | \n'),
    write('|              2. Player vs Computer1             | \n'),
    write('|              3. Player vs Computer2             | \n'), 
    write('|              4. Computer1 vs Computer1          | \n'),
    write('|              5. Computer1 vs Computer2          | \n'),
    write('|              6. Computer2 vs Computer2          | \n'),
    write('|              7. Exit                            | \n'), 
    write('|                                                 | \n'),  
    write('|                                                 | \n'),   
    write(' ------------------------------------------------- \n').  


choose_main_menu(1):- assert(player(human,1,0)), assert(player(human,2,0)), play_game().   
choose_main_menu(2):- assert(player(human,1,0)), assert(player(computer,2,1)), play_game().
choose_main_menu(3):- assert(player(human,1,0)), assert(player(computer,2,2)), play_game().
choose_main_menu(4):- assert(player(computer,1,1)), assert(player(computer,2,1)), play_game().
choose_main_menu(5):- assert(player(computer,1,1)), assert(player(computer,2,2)), play_game().
choose_main_menu(6):- assert(player(computer,1,2)), assert(player(computer,2,2)), play_game().
choose_main_menu(7):- halt.



% ------------------------- DISPLAY GAME --------------------------------
display_game(GameState) :-
    display_header,
    display_game_aux(GameState, 0).

display_header :-
    write('    -------------------------------------'), nl,
    write('    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |'), nl,
    write('-----------------------------------------'), nl.   

display_game_aux([], _) :- !.
display_game_aux([Row | Tail], N) :-
    format('| ~d | ', [N]),
    display_row(Row), nl,
    write('-----------------------------------------'), nl,
    N1 is N+1,
    display_game_aux(Tail, N1).

display_row([]) :- !.
display_row([H | T]) :- 
    display_cell(H), write(' | '),
    display_row(T).

display_cell(H) :-
    (H = 0 -> write('.');
    H = 1 -> write('X');
    H = 2 -> write('O')).


congratulate(Winner) :-
    player(Type, Winner, _), !,
    format('Congratulations, you won ~w ~d!', [Type, Winner]), nl.





% ------------------------- Auxiliary Functions ----------------------------

nth(0, [H|_], H) :- !.
nth(N, [_|T], X) :-
    N1 is N-1,
    nth(N1, T, X).


replace(0, [_|T], X, [X|T]) :- !.
replace(N, [H|T], X, [H|T1]) :-
    N1 is N-1,
    replace(N1, T, X, T1).


