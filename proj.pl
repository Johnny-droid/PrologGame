:- consult('board.pl').

% GameState is a list of lists (9x9)

% ------------------------- GAME MAIN STRUCTURE --------------------------------
play :-
    display_main_menu,
    write('Enter your option: '), read(Option),
    choose_main_menu(Option).

play_game(Player1, Player2):-
    initial_state(GameState),
    display_game(GameState).
    %game_cycle(GameState-Player).

game_cycle(GameState-Player):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player):-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(GameState-NextPlayer), !,
    game_cycle(NewGameState-NextPlayer).



% ------------------------- GAME LOGIC --------------------------------
choose_move(GameState, human-X, Move):- % still needs to check if move is valid
    format('Player ~d turn!', [X]), nl,
    write('Enter row: '), read(Row),
    write('Enter column: '), read(Column),
    Move = Row-Column.

choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).

valid_moves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).

choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).

choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, 
        NewState^( member(Mv, Moves),
        move(GameState, Mv, NewState),
        evaluate_board(NewState, Value) ), [_V-Move|_]).


game_over(GameState, Winner) :- 
    nth(4, GameState, Row),
    nth(4, Row, Winner), (Winner \= 0), !.


% ------------------------- GAME MENUS --------------------------------

display_main_menu :- 
    write(' ------------------------------------------------- \n'),
    write('|                                                 | \n'),  
    write('|                     CENTER                      | \n'),
    write('|                                                 | \n'),   
    write('|               1. Player vs Player               | \n'),
    write('|               2. Player vs Computer             | \n'), 
    write('|               3. Computer vs Computer           | \n'), 
    write('|               4. Exit                           | \n'), 
    write('|                                                 | \n'),  
    write('|                                                 | \n'),   
    write(' ------------------------------------------------- \n').  


choose_main_menu(1):- play_game(human-1, human-2).   
choose_main_menu(2):- play_game(human-1, computer-2).
choose_main_menu(3):- play_game(computer-1, computer-2).
choose_main_menu(4):- halt.



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
    format('Congratulations, you won Player ~d!', [Winner]), nl.





% ------------------------- Auxiliary Functions ----------------------------

nth(0, [H|_], H) :- !.
nth(N, [_|T], X) :-
    N1 is N-1,
    nth(N1, T, X).




