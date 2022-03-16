/** <module> Lexer to produce labeled tokens */

:- module(lexer, [
    alnum_tokens/3,
    word_tokens/3
    ]
).

%! alnum_tokens(Tokens:list)
% Match a list of alphanumeric tokens.
alnum_tokens([Token|Tokens]) -->
    alnum_token(Token),
    alnum_tokens(Tokens).
alnum_tokens(Tokens) -->
    [S],
    {char_type(S, space)},
    alnum_tokens(Tokens).
alnum_tokens([]) -->  [].

%! alnum_token(Token:comp)
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

%! word_tokens(Tokens:list)
% Match a list of word tokens, distinguishing upper and lowercase words.
word_tokens([Token|Tokens]) -->
    word_token(Token),
    word_tokens(Tokens).
word_tokens(Tokens) -->
    [S],
    {char_type(S, space)},
    word_tokens(Tokens).
word_tokens([]) -->  [].

%! word_token(Token:comp)
% Match a single word token.
word_token(word(W, upper)) -->
    char(C, upper),
    chars(Cs, alpha),
    !,
    {atom_chars(W, [C|Cs])}.
word_token(word(W, lower)) -->
    char(C, lower),
    chars(Cs, alpha),
    !,
    {atom_chars(W, [C|Cs])}.
word_token(number(N)) -->
    char(D, digit),
    chars(Ds, digit),
    !,
    {atom_chars(N, [D|Ds])}.
word_token(mark(M)) -->
    char(P, punct),
    !,
    {atom_chars(M, [P])}.

%! char(C:char)
% Match a single character.
char(C, Type) -->
    [C],
    {char_type(C, Type)}.

%! chars(Cs:list<char>)
% Match a list of characters.
chars([C|Cs], Type) -->
    char(C, Type),
    chars(Cs, Type).
chars([], _) --> [].
