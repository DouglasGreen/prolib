%%
% <module> Prolog linter
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_linter, [
        check/1
    ]
).
:- use_module(prolog_lexer).
:- use_module(prolog_oplist).

check([Token|Tokens]) :-
	writeln(Token),
	check(Tokens).
check([]).

