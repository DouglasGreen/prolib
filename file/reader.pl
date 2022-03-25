%%
% <module> File reader
%
% @author Douglas S. Green
% @license GPL

:- module(reader, [
        read_lines/2
    ]
).

%! read_lines(Tokens:list)
% Read the lines of a file to a list.
read_lines(File, Lines) :-
    setup_call_cleanup(
        open(File, read, In),
        bagof(Line, In^stream_line(In, Line), Lines),
        close(In)
    ).

%! stream_line(+In:stream, -Line:string)
% Read a line from the stream.
stream_line(In, Line) :-
    repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  Line0 = Line
    ;   !,
        fail
    ).

