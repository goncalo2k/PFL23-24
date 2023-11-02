/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */



% Declare dynamic predicates for active discs and board state
:- dynamic active_disc/2.
:- dynamic board/1.

% Initialize the board with empty hexagons
initialize_board(Board) :-
    Board = [
        ['.','.','.','.','.'],
        ['.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.','.'],
        ['.','.','.','.','.']
    ].

% Initialize the active discs for each player
initialize_active_discs(Player) :-
    retractall(active_disc(_, _)),
    asserta(active_disc(Player, [1, 1])),
    asserta(active_disc(Player, [1, 2])),
    asserta(active_disc(Player, [2, 1])),
    asserta(active_disc(Player, [2, 2])),
    asserta(active_disc(Player, [3, 1])),
    asserta(active_disc(Player, [3, 2])),
    asserta(active_disc(Player, [4, 1])),
    asserta(active_disc(Player, [4, 2])),
    asserta(active_disc(Player, [5, 1])),
    asserta(active_disc(Player, [5, 2])).

% Predicate to find the Nth element in a list
nth1(1, [X|_], X).
nth1(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth1(N1, T, X).

% Custom implementation of nth1/4 predicate
nth1(1, [Element|_], Element, _).
nth1(N, [_|Tail], Element, Index) :-
    N > 1,
    NewIndex is Index + 1,
    nth1(N-1, Tail, Element, NewIndex).


replace([Row|Rows], 1, Column, NewColumn, [NewRow|Rows]) :-
    replace_row(Row, Column, NewColumn, NewRow).

replace([Row|Rows], RowIndex, Column, NewColumn, [Row|NewRows]) :-
    RowIndex > 1,
    NextRowIndex is RowIndex - 1,
    replace(Rows, NextRowIndex, Column, NewColumn, NewRows).

% Predicate to replace a hexagon with a disc in the board
replace_hex(Board, [X, Y], Disc, NewBoard) :-
    replace_row(Board, X, Row, NewRow),
    replace_nth1(Y, Row, Disc, UpdatedRow),
    replace_nth1(X, Board, UpdatedRow, NewBoard).

% Predicate to replace the Nth element in a list
replace_nth1(1, [_|T], X, [X|T]).
replace_nth1(N, [H|T], X, [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace_nth1(N1, T, X, R).

% Predicate to replace a row in the board
replace_row([_|T], 1, X, [X|T]).
replace_row([H|T], N, X, [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace_row(T, N1, X, R).


% Predicate to place discs on the board during the placement phase
place_disc(Board, [X, Y], Disc, NewBoard) :-
    nth1(X, Board, Row, RestBoard),
    nth1(Y, Row, '.', NewRow),
    replace_row(Board, X, NewRow, TempBoard),
    replace_nth1(X, TempBoard, NewRow, NewBoard),
    asserta(active_disc(Disc, [X, Y])).

% Predicate to move a disc on the board during the movement phase
move_disc(Board, [X1, Y1], [X2, Y2], NewBoard) :-
    nth1(X1, Board, Row1),
    nth1(Y1, Row1, Disc),
    nth1(X2, Board, Row2),
    nth1(Y2, Row2, Target),
    (Target = '.' ; Target = Disc),
    retract(active_disc(Disc, [X1, Y1])),
    asserta(active_disc(Disc, [X2, Y2])),
    replace_hex(Board, [X1, Y1], '.', TempBoard),
    replace_hex(TempBoard, [X2, Y2], Disc, NewBoard).

% Predicate to perform a bounce move
bounce(Board, [X1, Y1], [X2, Y2], NewBoard) :-
    valid_bounce(Board, [X1, Y1], [X2, Y2]),
    move_disc(Board, [X1, Y1], [X2, Y2], NewBoard).

% Predicate to check if a bounce move is valid
valid_bounce(Board, [X1, Y1], [X2, Y2]) :-
    valid_move(Board, [X1, Y1], [X2, Y2]),
    abs(X2 - X1) =< 1,
    abs(Y2 - Y1) =< 1.

% Predicate to check if a move is valid
valid_move(Board, [X1, Y1], [X2, Y2]) :-
    active_disc(_, [X1, Y1]),
    valid_destination([X1, Y1], [X2, Y2]),
    \+occupied_by_opponent([X1, Y1], [X2, Y2]).

% Predicate to check if a destination is valid
valid_destination([X1, Y1], [X2, Y2]) :-
    XDiff is abs(X2 - X1),
    YDiff is abs(Y2 - Y1),
    (XDiff = 1, YDiff = 0 ; XDiff = 0, YDiff = 1 ; XDiff = 1, YDiff = 1).

% Predicate to check if a position is occupied by an opponent's disc
occupied_by_opponent([X1, Y1], [X2, Y2]) :-
    active_disc(Player, [X1, Y1]),
    active_disc(OtherPlayer, [X2, Y2]),
    Player \= OtherPlayer.

% Predicate to check if a player has won
win(Player) :-
    findall(Position, active_disc(Player, Position), Positions),
    check_win_condition(Positions).

check_win_condition(Positions) :-
    member([X, Y], Positions),
    adjacent_positions([X, Y], Adjacent),
    member([X2, Y2], Adjacent),
    member([X3, Y3], Adjacent),
    X \= X2, Y \= Y2,
    X \= X3, Y \= Y3,
    active_disc(_, [X2, Y2]),
    active_disc(_, [X3, Y3]),
    !.

% Predicate to find adjacent positions of a given position
adjacent_positions([X, Y], Adjacent) :-
    X1 is X - 1, Y1 is Y - 1,
    X2 is X - 1, Y2 is Y,
    X3 is X, Y3 is Y - 1,
    X4 is X, Y4 is Y + 1,
    X5 is X + 1, Y5 is Y,
    X6 is X + 1, Y6 is Y + 1,
    Adjacent = [[X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4], [X5, Y5], [X6, Y6]].

% Predicate to switch players
switch_player(black, white).
switch_player(white, black).

% Public predicate to start the game
start_game :-
    initialize_board(Board),
    initialize_active_discs(black),
    initialize_active_discs(white),
    display_board(Board),
    play_turn(Board, black).

% Predicate to handle a player's turn
play_turn(Board, Player) :-
    format('~nPlayer ~w\'s turn.~n', [Player]),
    read_move(From, To),
    (valid_move(Board, From, To) ->
        move_disc(Board, From, To, NewBoard),
        display_board(NewBoard),
        (win(Player) ->
            format('Player ~w wins!~n', [Player]);
            switch_player(Player, NextPlayer),
            play_turn(NewBoard, NextPlayer)
        );
        write('Invalid move. Try again.~n'),
        play_turn(Board, Player)
    ).

% Predicate to read the player's move
read_move(From, To) :-
    write('Enter starting position (Row Col): '),
    read([Row1, Col1]),
    write('Enter ending position (Row Col): '),
    read([Row2, Col2]),
    From = [Row1, Col1],
    To = [Row2, Col2].

% Display the current state of the board
display_board(Board) :-
    nl,
    display_rows(Board, 0).

display_rows([], _).
display_rows([Row|Rest], N) :-
    print_spaces(N),
    print_row(Row),
    nl,
    NextN is N + 1,
    display_rows(Rest, NextN).

print_row([]).
print_row([Cell|Rest]) :-
    write(' '),
    print_cell(Cell),
    print_row(Rest).

print_cell('.') :- write('.').
print_cell('B') :- write('B').
print_cell('W') :- write('W').

print_spaces(0).
print_spaces(N) :-
    write(' '),
    NextN is N - 1,
    print_spaces(NextN).
