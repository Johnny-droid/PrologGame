:- use_module(library(between)).
:- use_module(library(random)).

:- dynamic player/3.

:- consult('board.pl').

% GameState is a list of lists (9x9)

% ------------------------- GAME MAIN STRUCTURE --------------------------------
play :-
    display_main_menu,
    write('Enter your option: '), read(Option),
    choose_main_menu(Option).

play_game(Player, NextPlayer) :-
    initial_state(GameState),
    display_game(GameState),
    game_cycle(GameState, Player-NextPlayer).

game_cycle(GameState, Player-NextPlayer):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState, Player-NextPlayer):-
    choose_move(GameState, Player, Move),
    move(GameState, Player, Move, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState, NextPlayer-Player).



% ------------------------- GAME LOGIC --------------------------------
choose_move(GameState, player(human, X, _), Move):- % still needs to check if move is valid. Probably use repeat here
    format('Player ~d turn!', [X]), nl,
    write('Enter row: '), read(Row),
    write('Enter column: '), read(Column),
    Column >= 0, Column < 9,
    Row >= 0, Row < 9,
    Move = Row-Column.

choose_move(GameState, player(computer, X, Level), Move):-
    valid_moves(GameState, player(computer, X, Level), Moves),
    choose_move_computer(GameState, player(computer, X, Level), Moves, Move).

valid_moves(GameState, Player, Moves):-
    findall(
        Row-Column, 
        (between(0,8,Row),
        between(0,8,Column),
        move(GameState, Player, Row-Column, NewState)),
        Moves).

% Level 1
choose_move_computer(_GameState, player(computer,X,1), Moves, Move):-
    random_select(Move, Moves, _Rest).

% Level 2
choose_move_computer(GameState, player(computer,X,2), Moves, Move):-
    setof(Value-Mv, 
        NewState^( member(Mv, Moves),
        move(GameState, Mv, player(computer,X,2), NewState),
        evaluate_board(NewState, Value) ), [_V-Move|_]).


move(GameState, player(_, N, _), Row-Column, NewGameState):- % needs to validate move
    nth(Row, GameState, RowList),
    nth(Column, RowList, 0),
    valid_move(GameState, Row-Column, player(_, N, _)),
    replace(Column, RowList, N, NewRowList),
    replace(Row, GameState, NewRowList, NewGameState).


valid_move(GameState, Row-Column, Player):- 
    check_horizontal(GameState, Row-Column, Player, Value1),
    check_vertical(GameState, Row-Column, Player, Value2),
    check_diagonal1(GameState, Row-Column, Player, Value3),
    check_diagonal2(GameState, Row-Column, Player, Value4),
    border_distance(Row-Column, Distance),
    Value is Value1+Value2+Value3+Value4,
    Distance @=< Value.



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


choose_main_menu(1):- play_game(player(human, 1, 0), player(human, 2, 0)), !.
choose_main_menu(2):- play_game(player(human, 1, 0), player(computer, 2, 1)), !.
choose_main_menu(3):- play_game(player(human, 1, 0), player(computer, 2, 2)), !.
choose_main_menu(4):- play_game(player(computer, 1, 1), player(computer, 2, 1)), !.
choose_main_menu(5):- play_game(player(computer, 1, 1), player(computer, 2, 2)), !.
choose_main_menu(6):- play_game(player(computer, 1, 2), player(computer, 2, 2)), !.
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
    format('Player ~d won!', [Winner]), nl.





% ------------------------- Auxiliary Functions ----------------------------

nth(0, [H|_], H) :- !.
nth(N, [_|T], X) :-
    N1 is N-1,
    nth(N1, T, X).

nthMatrix(Row-Column, GameState, Value) :-
    nth(Row, GameState, RowList),
    nth(Column, RowList, Value).


replace(0, [_|T], X, [X|T]) :- !.
replace(N, [H|T], X, [H|T1]) :-
    N1 is N-1,
    replace(N1, T, X, T1).


check_direction(RowStart-ColumnStart, RowDirection-ColumnDirection, GameState, Player, Value):-
    RowStart1 is RowStart+RowDirection,
    ColumnStart1 is ColumnStart+ColumnDirection,
    check_direction_aux(RowStart1-ColumnStart1, RowDirection-ColumnDirection, GameState, Player, Value).


check_direction_aux(RowStart-ColumnStart, _, _, _, Value) :-
    (RowStart < 0; RowStart > 8; ColumnStart < 0; ColumnStart > 8), !, Value is 0.


check_direction_aux(RowStart-ColumnStart, RowDirection-ColumnDirection, GameState, player(_, N, _), Value):-
    nthMatrix(RowStart-ColumnStart, GameState, ValueTemp),
    (ValueTemp == N -> Value is 1;
    ValueTemp \== 0 -> Value is 0;
    RowStart1 is RowStart+RowDirection,
    ColumnStart1 is ColumnStart+ColumnDirection,
    check_direction_aux(RowStart1-ColumnStart1, RowDirection-ColumnDirection, GameState, player(_, N, _), Value)).

check_horizontal(GameState, Row-Column, Player, Value):-
    check_direction(Row-Column, 0-(-1), GameState, Player, ValueLeft),
    check_direction(Row-Column, 0-1, GameState, Player, ValueRight),
    Value is ValueLeft+ValueRight.

check_vertical(GameState, Row-Column, Player, Value):-
    check_direction(Row-Column, 1-0, GameState, Player, ValueUp),
    check_direction(Row-Column, -1-0, GameState, Player, ValueDown),
    Value is ValueUp+ValueDown.

check_diagonal1(GameState, Row-Column, Player, Value):-
    check_direction(Row-Column, -1-1, GameState, Player, ValueUpRight),
    check_direction(Row-Column, 1-(-1), GameState, Player, ValueDownLeft),
    Value is ValueUpRight+ValueDownLeft.

check_diagonal2(GameState, Row-Column, Player, Value):-
    check_direction(Row-Column, -1-(-1), GameState, Player, ValueUpLeft),
    check_direction(Row-Column, 1-1, GameState, Player, ValueDownRight),
    Value is ValueUpLeft+ValueDownRight.

border_distance(Row-Column, Distance):-
    Row1 is Row-0,
    Row2 is 8-Row,
    Col1 is Column-0,
    Col2 is 8-Column,
    MinRow is min(Row1, Row2),
    MinColumn is min(Col1, Col2),
    Distance is min(MinRow, MinColumn).

