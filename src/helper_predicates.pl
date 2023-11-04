/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


upper_lists(S, L, UpperLists) :-
    generate_lists(S, L, Lists),
    reverse(Lists, UpperLists).

lower_lists(S, L, LowerLists) :-
    generate_lists(S, L, LowerLists).

middle_list(Size, List) :-
    length(List, Size),
    fill_list(Size, 0, List).

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
    fill_list(Length, 0, List).

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

write_n_times(Text, N) :-
    N >= 1,
    N1 is N - 1,
    write(Text),
    write_n_times(Text, N1).
write_n_times(_, _).

write_every_six(Current, Size, Text1, Text2) :-
    Current =< Size,
    Rest is Current mod 6,
    Rest = 0,
    write(Text1),
    Next is Current + 1,
    write_every_six(Next, Size, Text1, Text2).

write_every_six(Current, Size, Text1, Text2) :-
    Current =< Size,
    Rest is Current mod 6,
    Rest \= 0,
    write(Text2),
    Next is Current + 1,
    write_every_six(Next, Size, Text1, Text2).

write_every_six(_, _, _, _).

write_every_three(Current, Size, Text1, Text2, Text3) :-
    Current =< Size,
    Rest is Current mod 6,
    Rest = 0,
    write(Text1),
    Next is Current + 1,
    write_every_three(Next, Size, Text1, Text2, Text3).

write_every_three(Current, Size, Text1, Text2, Text3) :-
    Current =< Size,
    Rest is Current mod 3,
    Rest = 0,
    write(Text3),
    Next is Current + 1,
    write_every_three(Next, Size, Text1, Text2, Text3).

write_every_three(Current, Size, Text1, Text2, Text3) :-
    Current =< Size,
    Rest is Current mod 3,
    Rest \= 0,
    write(Text2),
    Next is Current + 1,
    write_every_three(Next, Size, Text1, Text2, Text3).

write_every_three(_,_,_,_,_).

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
    position_state(Element, ReturnList),
    last(ReturnList, Important),
    write_piece(Important).

write_piece_lower(Element) :-
    position_state(Element, ReturnList),
    reverse(ReturnList, Reversed),
    last(Reversed, Important),
    write_piece(Important).
    

write_piece(Piece) :-
    Piece = 0,
    write(' ').

write_piece(Piece) :-
    Piece = white,
    translate(w, Symbol),
    write(Symbol).

write_piece(Piece) :-
    Piece = black,
    translate(b, Symbol),
    write(Symbol).

write_piece(_).