%%
% <module> Lexer to produce labeled tokens
%
% @author Douglas S. Green
% @license GPL

:- module(lexer, [
        all_tokens/3,
        alnum_tokens/3,
        alpha_tokens/3
    ]
).

:- use_module(library(dcg/basics)).

%! all_tokens(Tokens:list)
% Match a list of all tokens including whitespace.
all_tokens([Token|Tokens]) -->
    all_token(Token),
    all_tokens(Tokens).
all_tokens([]) -->  [].

%! alnum_tokens(Tokens:list)
% Match a list of alphanumeric tokens.
alnum_tokens([Token|Tokens]) -->
    alnum_token(Token),
    alnum_tokens(Tokens).
alnum_tokens(Tokens) -->
    blank,
    blanks,
    alnum_tokens(Tokens).
alnum_tokens([]) -->  [].

%! alpha_tokens(Tokens:list)
% Match a list of alpha or numeric tokens.
alpha_tokens([Token|Tokens]) -->
    alpha_token(Token),
    alpha_tokens(Tokens).
alpha_tokens(Tokens) -->
    blank,
    blanks,
    alpha_tokens(Tokens).
alpha_tokens([]) -->  [].

%! all_token(Token:compound)
% Match a single token including whitespace.
all_token(space(N)) -->
    ` `,
    all_token(space(N1)),
    {N is N1 + 1}.
all_token(space(1)) -->
    ` `,
    !.
all_token(tab(N)) -->
    `\t`,
    all_token(tab(N1)),
    {N is N1 + 1}.
all_token(tab(1)) -->
    `\t`,
    !.
all_token(nl(N)) -->
    `\n`,
    all_token(nl(N1)),
    {N is N1 + 1}.
all_token(nl(1)) -->
    `\n`,
    !.
all_token(Token) -->
    `\r`,
    all_token(Token).
all_token(Token) -->
    alpha_token(Token).

%! alnum_token(Token:compound)
% Match a single alphanumeric token.
alnum_token(alnum(W)) -->
    char(C, alnum),
    chars(Cs, alnum),
    !,
    {atom_chars(W, [C|Cs])}.
alnum_token(mark(M)) -->
    char(P, punct),
    !,
    {atom_chars(M, [P])}.

%! alpha_token(Token:compound)
% Match a single alpha or numeric token.
alpha_token(alpha(W)) -->
    char(C, alpha),
    chars(Cs, alpha),
    !,
    {atom_chars(W, [C|Cs])}.
alpha_token(number(N)) -->
    char(D, digit),
    chars(Ds, digit),
    !,
    {atom_chars(N, [D|Ds])}.
alpha_token(mark(M)) -->
    char(P, punct),
    !,
    {atom_chars(M, [P])}.

%! chars(Cs:codes)
% Match a list of character codes.
chars([C|Cs], Type) -->
    char(C, Type),
    chars(Cs, Type).
chars([], _) --> [].

%! char(C:code)
% Match a single character code.
char(C, Type) -->
    [C],
    {char_type(C, Type)}.
