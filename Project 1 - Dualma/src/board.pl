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

move(X1,Y1,X2,Y2,Board,NewBoard) :-
    nth0(X1, Board, Row1),
    nth0(Y1, Row1, Cell1),
    check_height(Cell1,H1),
    get_reachable(Board,X1,Y1,H1,Reachable),
    (memberchk((X2,Y2),Reachable) ->
        (check_piece_ownership(Cell1) -> 
            (remove_element(Cell1,Val, NewCell),
            replace(Row1,Y1,NewCell,NewRow1),
            replace(Board,X1,NewRow1,TempBoard),
            nth0(X2, TempBoard, Row2),
            nth0(Y2, Row2, Cell2),
            check_height(Cell2, H2),
            (H2 < 2 -> add_element(Cell2, Val, NewCell3),
            replace(Row2,Y2,NewCell3,NewRow2),
            replace(TempBoard,X2,NewRow2,NewBoard),
            player_switcher; write('Too tall - try another cell!'),nl, append([],Board,NewBoard))
            )
        ;
            (write('Invalid move, try again - you dont own this piece.'), nl,
            append([],Board,NewBoard)))
    ;
    write('Invalid move, try again - the piece you are trying to reach is not reachable.'), nl,
            append([],Board,NewBoard)
    ).

    
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
    read(Temp1),
    verify_input(Temp1, 0, 15, X1),
    write('Y1:'),
    read(Temp2),
    verify_input(Temp2, 0, 15, Y1),
    write('X2:'),
    read(Temp3),
    verify_input(Temp3, 0, 15, X2),
    write('Y2:'),
    read(Temp4),
    verify_input(Temp4, 0, 15, Y2).

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
