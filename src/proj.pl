:- use_module(library(between)).
:- use_module(library(random)).

:- dynamic player/3.

:- consult('board.pl').

% GameState is a list of lists (9x9)

% ------------------------- GAME MAIN STRUCTURE --------------------------------
play :-
    display_main_menu,
    choose_player(1, Player1),
    choose_player(2, Player2),
    play_game(Player1, Player2).

play_game(Player, NextPlayer) :-
    initial_state(GameState),
    display_game(GameState),
    game_cycle(GameState, Player-NextPlayer).

game_cycle(GameState, _-_):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState, Player-NextPlayer):-
    repeat,
    choose_move(GameState, Player, Move),
    move(GameState, Player, Move, NewGameState), !,
    display_game(NewGameState), 
    game_cycle(NewGameState, NextPlayer-Player).



% ------------------------- GAME LOGIC --------------------------------
choose_move(_GameState, player(human, X, _), Move):- 
    format('Player ~d turn!', [X]), nl,
    write('Enter row: '), read(Row),
    write('Enter column: '), read(Column), nl, nl,
    Column >= 0, Column < 9,
    Row >= 0, Row < 9,
    Move = Row-Column.

choose_move(GameState, player(computer, X, Level), Move):-
    format('Player ~d turn! (LvL ~d)\n\n', [X, Level]), nl,
    valid_moves(GameState, player(computer, X, Level), Moves),
    choose_move_computer(GameState, player(computer, X, Level), Moves, Move).

valid_moves(GameState, Player, Moves):-
    findall(
        Row-Column, 
        (between(0,8,Row),
        between(0,8,Column),
        move(GameState, Player, Row-Column, _NewGameState)),
        Moves).


choose_move_computer(_GameState, _, [], _Move) :- 
    write('No valid moves!'), nl.

% Level 1
choose_move_computer(_GameState, player(computer,_,1), Moves, Move) :-
    random_select(Move, Moves, _Rest).

% Level 2 and 3
choose_move_computer(GameState, player(computer,X,Level), Moves, Move):-
    Level \== 1,
    setof(Value-Mv, 
        NewState^( member(Mv, Moves),
        move(GameState, player(computer,X,Level), Mv, NewState),
        value(NewState, player(computer,X,Level), Value) ), MovesWithValue),
    last(MovesWithValue, MaxValue-_),
    filter_max_value(MaxValue, MovesWithValue, MovesWithHighestValue),
    random_select(Move, MovesWithHighestValue, _Rest).



move(GameState, player(_, N, _), Row-Column, NewGameState):-
    nth(Row, GameState, RowList),
    nth(Column, RowList, 0),
    valid_move(GameState, Row-Column, player(_, N, _)),
    replace(Column, RowList, N, NewRowList),
    replace(Row, GameState, NewRowList, NewGameState).


valid_move(GameState, Row-Column, Player):- 
    check_all_directions(GameState, Row-Column, Player, Value),
    border_distance(Row-Column, Distance),
    Distance @=< Value.



game_over(GameState, Winner) :- 
    nth(4, GameState, Row),
    nth(4, Row, Winner), (Winner \== 0), !.




% ------------------------- GAME MENUS --------------------------------

display_main_menu :- 
    write(' -------------------------------------------------  \n'),
    write('|                                                 | \n'),  
    write('|                     CENTER                      | \n'), 
    write('|                                                 | \n'),   
    write(' -------------------------------------------------  \n'). 


choose_player(N, Player) :- 
    repeat,
    format('Choose player ~d: ', [N]), nl,
    write('1 - Human'), nl,
    write('2 - Computer'), nl,
    write('Enter your option: '), read(Option), nl,
    (Option = 1 -> Player = player(human, N, 0), !;
    Option = 2 -> choose_level(N, Player), !).

choose_level(N, Player) :-
    repeat,
    format('Choose level for player ~d: ', [N]), nl,
    write('1 - Easy'), nl,
    write('2 - Medium'), nl,
    write('3 - Hard'), nl,
    write('4 - Very Hard'), nl,
    write('Enter your option: '), read(Option), nl,
    (Option = 1 -> Player = player(computer, N, 1), !;
    Option = 2 -> Player = player(computer, N, 2), !;
    Option = 3 -> Player = player(computer, N, 3), !;
    Option = 4 -> Player = player(computer, N, 4), !).

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





% ------------------------- Direction Functions ----------------------------

check_all_directions(GameState, Row-Column, Player, Value) :-
    check_horizontal(GameState, Row-Column, Player, Value1),
    check_vertical(GameState, Row-Column, Player, Value2),
    check_diagonal1(GameState, Row-Column, Player, Value3),
    check_diagonal2(GameState, Row-Column, Player, Value4),
    Value is Value1+Value2+Value3+Value4.


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


% ------------------------- Evaluate Board ----------------------------
% evaluate used in level 4
value(GameState, player(Type, N, 4), Value) :-
    check_all_directions(GameState, 4-4, player(Type, N, 3) , ValuePlayer),
    (N = 1 -> N1 is 2; N1 is 1),
    check_all_directions(GameState, 4-4, player(Type, N1, 3) , ValueOpponent),
    Value is ValuePlayer-ValueOpponent.

% evaluate used in level 3
value(GameState, player(Type, N, 3), Value) :-
    check_all_directions(GameState, 4-4, player(Type, N, 3) , Value).

% evaluate used in level 2
value(GameState, player(Type, N, 2), Value):-
    value_aux(GameState, player(Type, N, 2), 0, 0, Value).

value_aux([], _Player, _RowNumber, Value, Value) :- !.
value_aux([Row | Tail], Player, RowNumber, Value, ValueFinal):-
    value_row(Row, RowNumber, 0, Player, 0, ValueRow),
    ValueMax is max(Value, ValueRow),
    RowNumber1 is RowNumber+1,
    value_aux(Tail, Player, RowNumber1, ValueMax, ValueFinal).

value_row([], _RowNumber, _ColumnNumber, _Player, Value, Value) :- !.
value_row([H | T], RowNumber, ColumnNumber, player(_, N, _), Value, ValueFinal):-
    (H == N -> border_distance(RowNumber-ColumnNumber, Distance), Value1 is max(Value, Distance);
    Value1 is Value),
    ColumnNumber1 is ColumnNumber+1,
    value_row(T, RowNumber, ColumnNumber1, player(_, N, _), Value1, ValueFinal).





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


last([X], X) :- !.
last([_|T], X) :- last(T, X).


filter_max_value(Max, MovesWithValue, Moves) :-
    filter_max_value_aux(Max, MovesWithValue, [], Moves).

filter_max_value_aux(_, [], Moves, Moves) :- !.

filter_max_value_aux(Max, [Value-Move | Tail], L, Moves) :-
    (Move == 4-4 -> filter_max_value_aux(Max, [], [4-4], Moves);
    Value >= Max -> filter_max_value_aux(Max, Tail, [Move | L], Moves);
    filter_max_value_aux(Max, Tail, L, Moves)).
