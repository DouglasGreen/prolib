

%! chunked(+Items, +Length, -Chunks).
% Separate a list into chunks of the given length.
chunked(Items, Length, [Chunk|Chunks]) :-
    Length > 0,
    length(Chunk, Length),
    append(Chunk, Rest, Items),
    Rest \= [],
    !,
    chunked(Rest, Length, Chunks).
chunked(Items, Length, [Items]) :-
    Length > 0,
    length(Items, Left),
    between(1, Length, Left).
chunked([], _, []).

:- begin_tests(prolib).

test(chunked) :-
    \+ chunked([a, b, c], 0, _),
    chunked([], _, []),
    chunked([a, b, c], 1, [[a], [b], [c]]),
    chunked([a, b, c], 2, [[a, b], [c]]),
    chunked([a, b, c], 3, [[a, b, c]]).

:- end_tests(prolib).
