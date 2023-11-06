/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


upper_lists(S, L, UpperLists) :-
    generate_lists(S, L, Lists),
    reverse(Lists, UpperLists).

lower_lists(S, L, LowerLists) :-
    generate_lists(S, L, LowerLists).

middle_list(Size, List) :-
    length(List, Size),
    fill_list(Size, [0,0], List).

generate_lists(Size, Length, Lists) :-
    generate_lists(Size, Length, [], Lists).

generate_lists(0, _, Acc, Acc).
generate_lists(Size, Length, Acc, Lists) :-
    Length >= 1,
    NewLength is Length - 1,
    generate_list(Length, List),
    append(Acc, [List], NewAcc),
    NewSize is Size - 1,
    generate_lists(NewSize, NewLength, NewAcc, Lists).

generate_list(Length, List) :-
    length(List, Length),
    fill_list(Length, [0,0], List).

fill_list(0, _, []).

fill_list(N, Element, [Element|Rest]) :-
    N > 0,
    N1 is N - 1,
    fill_list(N1, Element, Rest). 

get_elements(List, M, N, Elements) :-
    length(Prefix, M),
    append(Prefix, SubList, List),
    length(Elements, N),
    append(Elements, _, SubList).

reverse_list_helper([], Acc, Acc).
reverse_list_helper([Head|Tail], Acc, Reversed) :-
    reverse_list_helper(Tail, [Head|Acc], Reversed).

% replace(+List, +Index, +Value, -NewList)
% replaces the element at the given index starting from 0
replace([_|T], 0, Value, [Value|T]).
replace([H|T], Index, Value, [H|NewT]) :-
    Index > 0,
    NewIndex is Index - 1,
    replace(T, NewIndex, Value, NewT). 
write_n_times(Text, N) :-
    N >= 1,
    N1 is N - 1,
    write(Text),
    write_n_times(Text, N1).
write_n_times(_, _).

write_every_six(Current, Size, Text1, Text2) :-
    Current =< Size,
    Rest6 is Current mod 6,
    (Rest6 = 0 -> write(Text1); write(Text2)),
    Next is Current + 1,
    write_every_six(Next, Size, Text1, Text2).

write_every_six(_, _, _, _).


write_every_three(Current, Size, Text1, Text2, Text3) :-
    Current =< Size,
    Rest3 is Current mod 3,
    Rest6 is Current mod 6,
    (Rest6 = 0 -> write(Text1);
    Rest3 = 0 -> write(Text3);
    write(Text2)),
    Next is Current + 1,
    write_every_three(Next, Size, Text1, Text2, Text3).

write_every_three(_, _, _, _, _).


display_upper_line(List, Size, Symbol1, Symbol2, Symbol3, Symbol4, Symbol5) :-
    length(List, S),
    N is Size - S,
    write_n_times('   ', N),
    WriteLength is (S*6) - 1,
    write(Symbol1),
    write_every_three(1, WriteLength, Symbol2, Symbol4, Symbol5),
    write(Symbol3),
    nl.
    
    
write_upper_part(List, Size) :-
    length(List, S),
    N is Size - S,
    write_n_times('   ', N),
    vertical(Symbol),
    write(Symbol),
    write_line(List, 1),
    nl.

write_lower_part(List, Size) :-
    length(List, S),
    N is Size - S,
    write_n_times('   ', N),
    vertical(Symbol),
    write(Symbol),
    write_line(List, 2),
    nl.

write_line([], _).
write_line([First|Rest], N) :-
    First \= [],
    N = 1,
    vertical(Symbol),
    write('  '),
    write_piece_upper(First),
    write('  '),
    write(Symbol),
    write_line(Rest, N).

write_line([First|Rest], N) :-
    First \= [],
    N = 2,
    vertical(Symbol),
    write('  '),
    write_piece_lower(First),
    write('  '),
    write(Symbol),
    write_line(Rest, N).

write_piece_upper(Element) :-
    last(Element, Important),
    write_piece(Important).

write_piece_lower(Element) :-
    nth0(0, Element, Important),
    write_piece(Important).
    

write_piece(Piece) :-
    Piece = 0,
    write(' ').

write_piece(Piece) :-
    translate(Piece, Symbol),
    write(Symbol).

write_piece(_).

get_board_length([], 0).  
get_board_length([H|T], Sum) :-
    length(H, Length),
    get_board_length(T, RestSum),
    Sum is Length + RestSum.

manage_color_input(_, _, _,'b') :-
    write('Black it is!'), nl.

manage_color_input(_, _, _,'w') :-
    write('White it is!'), nl.

manage_color_input(Board,Size, N, _) :-
    write('Invalid color!'), nl,
    write('Please choose between "b" and "w".'), nl,
    placement_phase_loop(Board,Size, N),
    !.

coordenates_input(X, Y, Size) :-
        write('Row:'), read(Temp1), nl,
        verify_input(Temp1, 0, Size, X),
        write('Column:'), read(Temp2), nl,
        verify_input(Temp2, 0, Size, Y).
     
verify_input(Value, LowerBound,UpperBound, Return) :-
        number(Value),
        Value >= LowerBound,
        Value < UpperBound,
        Return is Value.

verify_input(_,LowerBound, UpperBound, Return) :-
        write('Invalid input, try again: '),
        read(Temp),
        verify_input(Temp, LowerBound, UpperBound, Return).

% get_reachable(+Board, +Row, +Column, +MaxDistance, -Reachable)
% returns a list of reachable cells within the given maximum distance, using a breadth-first search
get_reachable(Board, Row, Column, MaxDistance, Reachable) :-
    bfs(Board, [(Row, Column, 0)], MaxDistance, [(Row, Column)], Reachable).

% bfs(+Board, +Queue, +MaxDistance, +Visited, -Reachable)
% performs a breadth-first search on the board, starting from the given queue of cells
% the search is limited to the given maximum distance
% the visited cells are stored in the Visited list
% the reachable cells are stored in the Reachable list
bfs(_, [], _, _, []).
bfs(_, [(_, _, MaxDistance) | _], MaxDistance, _, []) :- !. % the maximum distance has been reached

bfs(Board, [(Row, Column, Distance) | Rest], MaxDistance, Visited, Reachable) :-
    Distance < MaxDistance, % the current cell is within the maximum distance
    get_adjacent(Board, Row, Column, Adjacent),                 % get the adjacent cells
    subtract(Adjacent, Visited, Possible),                      % remove the visited cells
    filter_unoccupied(Possible, Board, Unoccupied),             % remove the fully occupied cells

    NewDistance is Distance + 1,
    add_distance(Unoccupied, NewDistance, UnoccupiedWithDistance),  % associate the distance with each cell
    
    append(Rest, UnoccupiedWithDistance, NewRest),
    append(Visited, Unoccupied, NewVisited),
    
    bfs(Board, NewRest, MaxDistance, NewVisited, NewReachable),
    append(Unoccupied, NewReachable, Reachable)
    .


offsets([(1, 0), (-1, 0), (-1, -1), (0, -1), (0, 1), (-1, 1)]).
% get_adjacent(+Board, +Row, +Column, -Adjacent)
% returns a list of adjacent cells
get_adjacent(Board, Row, Column, Adjacent) :-
    offsets(Offsets),
    findall((AdjRow, AdjCol), (
        member((DX, DY), Offsets),
        AdjRow is Row + DY,
        AdjCol is Column + DX,
        get_cell(AdjRow, AdjCol, Board, _)
    ), Adjacent),
    write('adj exit').

% filter_unoccupied(+List, +Board, -NewList)
% filter_unoccupieds the given list of cells to only include cells that are not fully occupied
filter_unoccupied([], _, []).

filter_unoccupied([(Row, Column) | T], Board, [(Row, Column) | NewT]) :-
    get_cell(Row, Column, Board, Cell),
    check_height(Cell, Height),
    Height < 2, !,
    filter_unoccupied(T, Board, NewT).

filter_unoccupied([_ | T], Board, NewT) :-
    filter_unoccupied(T, Board, NewT).

% subtract(+List1, +List2, -NewList)
% subtracts the elements of the second list from the first list (List1\List2)
subtract([], _, []) :- !.

subtract([A|T], B, R) :-
    memberchk(A, B), !,
    subtract(T, B, R).

subtract([A|T], B, [A|R]) :-
    subtract(T, B, R).

add_distance([], _, []).

add_distance([(Row, Column) | T], Distance, [(Row, Column, Distance) | NewT]) :-
    add_distance(T, Distance, NewT).