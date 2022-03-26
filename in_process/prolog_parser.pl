%%
% <module> Prolog parser
%
% @author Douglas S. Green
% @license GPL


:- module(prolog_parser, [
        atom_names/3,
        clauses/3
    ]
).

:- use_module(prolog_lexer).

clauses([C|Cs]) -->
    clause(C),
    clauses(Cs).

clause(clause(H, B)) -->
    head(H),
    body(B).

head(term(H)) -->
    term(H).
head([]) -->
    [].

body(body(Gs)) -->
    `:-`,
    goals(Gs).

term(terms(Ts)) -->
    `(`,
    terms(Ts),
    `)`. 

atom_names([A|As]) -->
    atom_name(A),
    atom_names(As).
atom_names([]) -->  [].
atom_name(atom_name(A)) -->
    [single_quoted_string(A)];
    [lower(A)];
    [A].
