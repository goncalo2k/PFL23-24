/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


upper_lists(S, L, UpperLists) :-
    generate_lists(S, L, Lists),
    reverse_list(Lists, UpperLists).

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

reverse_list(List, Reversed) :-
    reverse_list_helper(List, [], Reversed).

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
