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

parse_atom(atom(lower(Atom))) -->
    [lower(Atom)].
parse_atom(atom(quoted(single, Atom))) -->
    [quoted(single, Atom)].

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

parse_head(head(Head)) -->
    parse_atom(Head).
    /*
parse_head(head(Head)) :-
    parse_compound(Head).*/

parse_section(comments([Comment|Comments])) -->
    parse_comment(Comment),
    parse_comments(Comments).
parse_section(clauses([Clause|Clauses])) -->
    parse_clause(Clause),
    parse_clauses(Clauses).

parse_sections([Section|Sections]) -->
    parse_section(Section),
    parse_sections(Sections).
parse_sections([]) -->
    [].

parse_clause(fact(Fact)) -->
    parse_head(Fact),
    [mark('.')].

parse_comment(comment(line(Line))) -->
    [comment(line, Line)].
parse_comment(comment(block(Block))) -->
    [comment(block, Block)].
