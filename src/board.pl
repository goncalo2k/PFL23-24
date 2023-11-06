:- use_module(library(lists)).
:- use_module(library(clpfd)).

%%%%% Helper Characters %%%%%

% vertical    (-VerticalSeparator  )
% horizontal  (-HorizontalSeparator)
% right_corner(-RightCorner)
% left_corner (-LeftCorner )
% down_corner (-DownCorner )
% up_corner   (-UpCorner   )

vertical(Symbol)     :- char_code(Symbol, 9474).
horizontal(Symbol)   :- char_code(Symbol, 9472).
top_right_corner(Symbol) :- char_code(Symbol, 9488).
top_left_corner(Symbol)  :- char_code(Symbol, 9484).
bot_right_corner(Symbol) :- char_code(Symbol, 9496).
bot_left_corner(Symbol)  :- char_code(Symbol, 9492).
down_corner(Symbol)  :- char_code(Symbol, 9516).
up_corner(Symbol)    :- char_code(Symbol, 9524).
white_disk(Symbol) :- char_code(Symbol, 9675).
black_disk(Symbol) :- char_code(Symbol, 9679).

% translate(+CellPiece, -WritableSymbol)
translate(0, ' ').
translate(2, X) :- char_code(X,  9675).

translate(1, X) :- char_code(X,  9679).

disc(black).
disc(white).

initial_state(Size, Board) :-
    S is (Size // 2),
    L is Size - 1,
    upper_lists(S, L, UpperLists),
    lower_lists(S, L, LowerLists),
    middle_list(Size, List),
    append(UpperLists, [List], Temp),
    append(Temp, LowerLists, Board).

display_game(Board, Size) :-
    MiddleIndex is (Size + 1) // 2,
    PreviousMiddle is MiddleIndex - 1,
    last(Board, Last),
    nth1(MiddleIndex, Board, Middle),
    get_elements(Board, 0, MiddleIndex, UpperRows),
    get_elements(Board, PreviousMiddle, MiddleIndex, LowerRows),
    display_upper_most_row(Last, Size),
    display_upper_rows(UpperRows, Size),
    display_row(Middle, Size),
    display_lower_rows(LowerRows, Size),
    display_lower_most_row(Last, Size).

place_piece(X,Y,Board,Size,Color,NewBoard) :-
    nl,
    is_not_middle(X,Y,Size, X1, Y1),
    nth0(X1, Board, Row),
    nth0(Y1, Row, Cell),
    (get_cell(X1,Y1,Board, [0,0]) ->
        (Color == 'w' -> 
            replace(Row,Y1,[1, 0],NewRow),
            replace(Board,X1,NewRow,NewBoard)
            ;
            replace(Row,Y1,[2, 0],NewRow),
            replace(Board,X1,NewRow,NewBoard)
        ) 
        ;
        (write('Invalid move, try again.'), nl,
        write('Where would you like to place your piece?'), nl,
        coordenates_input(NewX, NewY, Size),
        place_piece(NewX, NewY, Board, Size, Color, NewBoard))
    ).

move_piece(X1,Y1,X2,Y2,Board,NewBoard) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Cell1),
    (check_piece_ownership(Cell1) -> 
        (remove_element(Cell1,Val, NewCell),
        replace(Row1,Y1,NewCell,NewRow1),
        replace(Board,X1,NewRow1,TempBoard),
        nth0(X2, TempBoard, Row2),
        nth0(Y2, Row2, Cell2),
        check_height(Cell2, H),
        (H < 2 -> add_element(Cell2, Val, NewCell3),
        replace(Row2,Y2,NewCell3,NewRow2),
        replace(TempBoard,X2,NewRow2,NewBoard),
        player_switcher))
    ;
        append([],Board,NewBoard)).

    
check_height(Piece, H) :-
    nth0(1,Piece,TopVal),
    nth0(0,Piece,BotVal),
    (TopVal \= 0 -> H is 2 ; (BotVal \= 0 -> H is 1 ; H is 0))
    .


check_piece_ownership(Piece) :-
    nth0(1,Piece,TopVal),
    nth0(0,Piece,BotVal),
    current_player(Player),
    (TopVal == 0 -> ((BotVal == 2, Player == black); (BotVal == 1, Player == white))
     ; 
    ((TopVal == 2, Player == black);(TopVal == 1, Player == white))).
    
remove_element(Cell, ValueStill, NewCell) :-
    nth0(0,Cell,FirstValue),
    nth0(1,Cell,LastVal),
    (LastVal =:= 0 -> (ValueStill is FirstValue, replace(Cell,0,0,NewCell) ); (ValueStill is LastVal, replace(Cell,1,0,NewCell))).
    
add_element(Cell,Value, NewCell) :-
    nth0(0,Cell,FirstValue),
    nth0(1,Cell,LastVal),
    (FirstValue =:= 0 -> replace(Cell,0,Value,NewCell) ; replace(Cell,1,Value,NewCell)).

read_move(X1,Y1,X2,Y2) :-
    write('X1:'),
    read(X1),
    write('Y1:'),
    read(Y1),
    write('X2:'),
    read(X2),
    write('Y2:'),
    read(Y2).

display_upper_most_row(List, Size) :-
    length(List, S),
    N is Size - S,
    write_n_times('   ', N),
    WriteLength is (S*6) - 1,
    top_left_corner(Symbol1),
    down_corner(Symbol2),
    top_right_corner(Symbol3),
    horizontal(Symbol4),
    write(Symbol1),
    write_every_six(1, WriteLength, Symbol2, Symbol4),
    write(Symbol3),
    nl.

display_lower_most_row(List, Size) :-
    length(List, S),
    N is Size - S,
    write_n_times('   ', N),
    WriteLength is (S*6) - 1,
    bot_left_corner(Symbol1),
    up_corner(Symbol2),
    bot_right_corner(Symbol3),
    horizontal(Symbol4),
    write(Symbol1),
    write_every_six(1, WriteLength, Symbol2, Symbol4),
    write(Symbol3),
    nl.

display_upper_rows([Element|Rest], Size) :-
    Rest \= [],
    display_row(Element, Size),
    nth0(0, Rest, First),
    top_left_corner(Symbol1),
    down_corner(Symbol2),
    top_right_corner(Symbol3),
    horizontal(Symbol4),
    up_corner(Symbol5),
    display_upper_line(First, Size, Symbol1, Symbol2, Symbol3, Symbol4, Symbol5),
    display_upper_rows(Rest, Size).
display_upper_rows([_], _).

display_lower_rows([Element|Rest], Size) :-
    Rest \= [],
    bot_left_corner(Symbol1),
    up_corner(Symbol2),
    bot_right_corner(Symbol3),
    horizontal(Symbol4),
    down_corner(Symbol5),
    display_upper_line(Element, Size, Symbol1, Symbol2, Symbol3, Symbol4, Symbol5),
    nth0(0, Rest, First),
    display_row(First, Size),
    display_lower_rows(Rest, Size).
display_lower_rows([_], _).


display_row(List, Size) :-
    write_upper_part(List, Size),
    write_lower_part(List, Size).    


get_cell(X,Y,Board,Cell) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Cell).

is_cell_empty(Cell) :-
    Cell =:= 0.

is_not_middle(X,Y,Size, X1, Y1) :-
    Temp is Size//2,
    ((X == Temp, Y == Temp) ->
        write('You can\'t place a piece in the middle of the board!'), nl,
        write('Where would you like to place your piece?'), nl,
        coordenates_input(NewX, NewY, Size),
        is_not_middle(NewX, NewY, Size, X1, Y1)
        ;
        X1 = X, Y1 = Y).

    

/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- 



% Declare seed/1 as a dynamic predicate
:- dynamic seed/1.
seed(27).

% Representation of discs (black and white)
disc(black).
disc(white).

% Representation of the board
% '.' represents an empty hexagon, 'B' represents black disc, 'W' represents white disc
board([
    ['.','.','.','.','.'],
    ['.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.'],
    ['.','.','.','.','.']
]).

% Predicate to place discs on the board
place_discs([], Board, Board).
place_discs([Disc|Rest], Board, NewBoard) :-
    random_empty_hex(Board, Hex),
    replace_hex(Board, Hex, Disc, UpdatedBoard),
    place_discs(Rest, UpdatedBoard, NewBoard).

% Predicate to find a random empty hexagon on the board
random_empty_hex(Board, [X, Y]) :-
    length(Board, Rows),
    nth1(X, Board, Row),
    length(Row, Cols),
    random_between(1, Rows, X),
    random_between(1, Cols, Y),
    nth1(X, Board, Row),
    nth1(Y, Row, '.').

% Predicate to replace a hexagon with a disc
replace_hex(Board, [X, Y], Disc, NewBoard) :-
    replace_row(Board, X, NewRow, 1),
    replace_row(NewRow, Y, Disc, NewBoard, 1).

replace_row([_|T], Index, Element, [Element|T], Index).
replace_row([H|T], Index, Element, [H|R], CurrentIndex) :-
    NewIndex is CurrentIndex + 1,
    replace_row(T, Index, Element, R, NewIndex).

% Predicate to move a disc
move_disc(Board, [X1, Y1], [X2, Y2], NewBoard) :-
    nth1(X1, Board, Row1),
    nth1(Y1, Row1, Disc),
    nth1(X2, Board, Row2),
    nth1(Y2, Row2, Target),
    Disc \= '.',
    Target = '.',
    replace_hex(Board, [X1, Y1], '.', TempBoard),
    replace_hex(TempBoard, [X2, Y2], Disc, NewBoard).

% Predicate to check if a move is valid
valid_move(Board, [X1, Y1], [X2, Y2]) :-
    nth1(X1, Board, Row1),
    nth1(Y1, Row1, Disc),
    nth1(X2, Board, Row2),
    nth1(Y2, Row2, Target),
    Disc \= '.',
    Target = '.'.

% Predicate to check if a stack is active (height-2)
is_active_stack([_, _]).
is_active_stack([_,_|_]).

% Predicate to check if a stack is a height-2 stack
is_height_two_stack([_,_|_]).

% Predicate to check if a stack is a single disc (height-1)
is_single_disc_stack([_]).

% Predicate to perform a bounce move
bounce(Board, [X1, Y1], [X2, Y2], NewBoard) :-
    valid_bounce(Board, [X1, Y1], [X2, Y2]),
    nth1(X1, Board, Row1),
    nth1(Y1, Row1, Disc),
    replace_hex(Board, [X1, Y1], '.', TempBoard),
    move_disc(TempBoard, [X1, Y1], [X2, Y2], NewBoard).

% Predicate to check if a bounce move is valid
valid_bounce(Board, [X1, Y1], [X2, Y2]) :-
    valid_move(Board, [X1, Y1], [X2, Y2]),
    abs(X2 - X1) =< 1,
    abs(Y2 - Y1) =< 1.

% Predicate to check if a win condition is met
win(Board, Player) :-
    check_rows(Board, Player).
    % Add more win conditions if necessary.

% Predicate to check rows for a win condition
check_rows(Board, Player) :-
    member(Row, Board),
    check_row(Row, Player).

check_row(Row, Player) :-
    append(_, [Stack|_], Row),
    is_height_two_stack(Stack),
    member(Player, Stack).

nth1(1, [X|_], X).
nth1(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth1(N1, T, X).

replace_row([_|T], 1, X, [X|T]).
replace_row([H|T], N, X, [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace_row(T, N1, X, R).

% Get the current system time in milliseconds
current_time(Time) :-
    statistics(runtime, [Milliseconds|_]),
    Time is Milliseconds * 1000.

% Seed the random number generator with the current system time
seed_random :-
    current_time(Time),
    set_seed(Time).

% Set the seed for the random number generator
set_seed(Seed) :-
    NewSeed is Seed mod 30269,
    asserta(seed(NewSeed)).

% Generate a random number between Low and High
random_between(Low, High, Random) :-
    seed(Seed),
    NewSeed is (Seed * 16807) mod 2147483647,
    NewRandom is NewSeed mod (High - Low + 1),
    Random is NewRandom + Low,
    retractall(seed(_)),
    asserta(seed(NewSeed)).

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



% Make a move on the board
make_move(Board, Player, [X, Y], NewBoard) :-
    valid_move(Board, [X, Y]),
    move_disc(Board, [X, Y], NewBoard),
    update_active_player(Player).

% Move a disc to the specified position
move_disc(Board, [X, Y], NewBoard) :-
    nth1(X, Board, Row, RestBoard),
    nth1(Y, Row, Cell, NewRow),
    Cell \= '.',
    replace_hex(Board, [X, Y], '.', TempBoard),
    replace_row(TempBoard, X, NewRow, TempBoard2),
    replace_row(TempBoard2, X, Row, NewBoard).


% Predicate to check if a move is valid
valid_move(Board, [X, Y]) :-
    nth1(X, Board, Row, _),
    nth1(Y, Row, Cell),
    Cell \= '.'.

% Update active player (switch between black and white)
update_active_player(black).
update_active_player(white).

% Get the element at the specified index (1-based) in the list
nth1(Index, [Element|_], Element, 1) :- Index = 1.
nth1(Index, [_|Rest], Element, CurrentIndex) :-
    NewIndex is CurrentIndex - 1,
    nth1(Index, Rest, Element, NewIndex).

% Replace the element at the specified index (1-based) in the list
replace_nth1(Index, [_|Rest], Element, [Element|Rest], 1) :- Index = 1.
replace_nth1(Index, [H|Rest], Element, [H|NewRest], CurrentIndex) :-
    NewIndex is CurrentIndex - 1,
    replace_nth1(Index, Rest, Element, NewRest, NewIndex).

% Public predicate to replace the element at the specified index
replace_nth1(Index, List, Element, NewList) :-
    replace_nth1(Index, List, Element, NewList, _).


% Play the game
play :-
    seed_random, % Seed the random number generator
    board(Board), % Get the initial game board
    display_board(Board), % Display the initial game board
    play_turn(Board, black). % Start the first player's turn

% Predicate to handle a player's turn
play_turn(Board, Player) :-
    format('~nPlayer ~w\'s turn.~n', [Player]),
    read_move(Row, Col), % Read the player's move
    make_move(Board, Player, [Row, Col], NewBoard), % Make the move
    display_board(NewBoard), % Display the updated game board
    (win(NewBoard, Player) -> % Check if the player has won
        format('Player ~w wins!~n', [Player]);
        switch_player(Player, NextPlayer), % Switch to the next player
        play_turn(NewBoard, NextPlayer) % Start the next player's turn
    ).

% Predicate to read the player's move
read_move(Row, Col) :-
    write('Enter row (1-10): '),
    read(Row),
    write('Enter column (1-5): '),
    read(Col),
    valid_move_input(Row, Col), % Validate the input
    !. % Cut to prevent backtracking

% Predicate to validate the player's move input
valid_move_input(Row, Col) :-
    between(1, 10, Row),
    between(1, 5, Col).

% Custom implementation of between/3
between(Min, Max, Min) :- Min =< Max.
between(Min, Max, Value) :- 
    Min < Max,
    NextMin is Min + 1,
    between(NextMin, Max, Value).


% Predicate to switch between players
switch_player(black, white).
switch_player(white, black).

*/