%%
% <module> Prolog parser
%
% Graphical tokens haven't been implemented yet.
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_parser, [
        parse_file/3
    ]
).

:- use_module(prolog_lexer).
:- use_module(prolog_oplist).

parse_file(file(Sections)) -->
    parse_sections(Sections).

parse_sections([Section|Sections]) -->
    parse_section(Section),
    parse_sections(Sections).
parse_sections([]) -->
    [].

parse_section(comments([Comment|Comments])) -->
    parse_comment(Comment),
    parse_comments(Comments).
parse_section(clauses([Clause|Clauses])) -->
    parse_clause(Clause),
    parse_clauses(Clauses).

parse_comment(comment(line(Line))) -->
    [line_comment(Line)].
parse_comment(comment(block(Block))) -->
    [block_comment(Block)].

parse_clauses([Clause|Clauses]) -->
    parse_clause(Clause),
    parse_clauses(Clauses).
parse_clauses([]) -->
    [].

parse_comments([Comment|Comments]) -->
    parse_comment(Comment),
    parse_comments(Comments).
parse_comments([]) -->
    [].

parse_clause(fact(Fact)) -->
    parse_head(Fact),
    [mark('.')].

parse_head(head(Head)) -->
    parse_atom(Head).
    /*
parse_head(head(Head)) :-
    parse_compound(Head).*/

parse_atom(atom(lower(Atom))) -->
    [lower(Atom)].
parse_atom(atom(single_quoted(Atom))) -->
    [single_quoted(Atom)].

/*
clause(clause(Head, Block)) -->
    head(Head),
    body(Block).

head(term(Head)) -->
    term(Head).
head([]) -->
    [].

body(body(Goals)) -->
    [operator(':-', _)],
    goals(Goals).

goals([]) -->
    [].

term(term(Term)) -->
    variable(Term).
term(term(Terms)) -->
    termlist(Terms).

termlist([paren, Term|Terms]) -->
    [mark('(')],
    term(Term),
    terms(Terms),
    [mark(')')]. 
termlist([bracket, Term|Terms]) -->
    [mark('[')],
    term(Term),
    terms(Terms),
    [mark(']')]. 
termlist([brace, Term|Terms]) -->
    [mark('{')],
    term(Term),
    terms(Terms),
    [mark('}')]. 

terms([Term|Terms]) -->
    [mark(',')],
    term(Term),
    terms(Terms).
terms([]) -->
    [].

variable(variable(Var)) -->
    [upper(Var)].

*/
