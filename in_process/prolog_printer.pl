%%
% <module> Prolog printer
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_printer, [
        pretty_print/1
    ]
).

:- use_module(prolog_parser).
:- use_module(prolog_lexer).

pretty_print(file(Sections)) :-
    print_sections(Sections).

print_atom(single_quoted(Atom)) :-
    writeq(Atom).

print_clause(fact(head(Fact))) :-
    print_fact(Fact).

print_clauses([Clause|Clauses]) :-
    print_clause(Clause),
    print_clauses(Clauses).
print_clauses([]).

print_comment(comment(line(Line))) :-
    format('%~w\n', Line).
print_comment(comment(block(Block))) :-
    format('/*~w*/\n', Block).

print_comments([Comment|Comments]) :-
    print_comment(Comment),
    print_comments(Comments).
print_comments([]).

print_fact(atom(Atom)) :-
    print_atom(Atom),
    writeln(".\n").

print_section(clauses(Clauses)) :-
	print_clauses(Clauses).
print_section(comments(Comments)) :-
	print_comments(Comments).

print_sections([Section|Sections]) :-
    print_section(Section),
	print_sections(Sections).
print_sections([]).

