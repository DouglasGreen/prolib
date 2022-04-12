

%! list_chunks(+Items, +Length, -Chunks).
% Separate a list into chunks of the given length.
list_chunks(Items, Length, [Chunk|Chunks]) :-
    Length > 0,
    length(Chunk, Length),
    append(Chunk, Rest, Items),
    Rest \= [],
    !,
    list_chunks(Rest, Length, Chunks).
list_chunks(Items, Length, [Items]) :-
    Length > 0,
    length(Items, Left),
    between(1, Length, Left).
list_chunks([], _, []).

:- begin_tests(list_chunks).

test(list_chunks) :-
    \+ list_chunks([a, b, c], 0, _),
    list_chunks([], _, []),
    list_chunks([a, b, c], 1, [[a], [b], [c]]),
    list_chunks([a, b, c], 2, [[a, b], [c]]),
    list_chunks([a, b, c], 3, [[a, b, c]]).

:- end_tests(list_chunks).

%! list_counts(+Items, -Counts)
% Count a list by pairs.
list_counts(Items, Counts) :-
    msort(Items, Sorted),
    clumped(Sorted, Counts).

:- begin_tests(list_counts).

test(list_counts) :-
    list_counts([], []),
    list_counts([a, b], [a-1, b-1]),
    list_counts([b, a], [a-1, b-1]),
    list_counts([a, b, a], [a-2, b-1]),
    list_counts([a, b, a, c, b], [a-2, b-2, c-1]).

:- end_tests(list_counts).
