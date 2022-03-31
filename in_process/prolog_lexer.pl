%%
% <module> Prolog lexer
%
% Every token is given a name. See token//1.
%
% See:
% https://www.swi-prolog.org/pldoc/man?section=syntax
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_lexer, [
        tokens/3
    ]
).

:- use_module(library(dcg/basics)).

%! tokens(Tokens:list)
% Match a list of all tokens excluding whitespace.
tokens([Token|Tokens]) -->
    token(Token),
    tokens(Tokens).
tokens(Tokens) -->
    blank,
    blanks,
    tokens(Tokens).
tokens([]) -->
     [].

%! chars(Chars:codes, Type:atom|compound)
% Match a list of character codes.
chars([Char|Chars], Type) -->
    char(Char, Type),
    chars(Chars, Type).
chars([], _) -->
    [].

%! base_digit(Digit:code, Base:int)
% Match an digit character code in a base from 2 to 10.
base_digit(Digit, Base) -->
    [Digit],
    {
        char_type(Digit, digit(Weight)),
        Weight < Base
    }.

%! base_digits(Digits:codes)
% Match a list of one or more digit codes in a base from 2 to 10.
base_digits([Digit|Digits], Base) -->
    base_digit(Digit, Base),
    base_digits(Digits, Base).
base_digits([Digit], Base) -->
    base_digit(Digit, Base).

%! quoted_chars(Quote:atom, Chars:codes)
% Match a list of character codes up to a the end of a quoted string.
quoted_chars(Quote, [Char|Chars]) -->
    [Char],
    {
        \+ char_code('\\', Char),
        \+ char_code(Quote, Char)
    },
    quoted_chars(Quote, Chars).
quoted_chars(Quote, [Char1, Char2|Chars]) -->
    [Char1],
    {char_code('\\', Char1)},
    [Char2],
    {char_code('x', Char2)},
    hex_chars(HexChars),
    [Char3],
    {char_code('\\', Char3)},
    quoted_chars(Quote, Rest),
    {append(HexChars, [Char3|Rest], Chars)}.
quoted_chars(Quote, [Char|Chars]) -->
    [Char],
    {char_code('\\', Char)},
    base_digits(Digits, 8),
    quoted_chars(Quote, Rest),
    {append(Digits, Rest, Chars)}.
quoted_chars(Quote, [Char1, Char2, Char3, Char4, Char5, Char6|Chars]) -->
    [Char1],
    {char_code('\\', Char1)},
    [Char2],
    {char_code('u', Char2)},
    hex_char(Char3),
    hex_char(Char4),
    hex_char(Char5),
    hex_char(Char6),
    quoted_chars(Quote, Chars).
quoted_chars(Quote, [Char1, Char2, Char3, Char4, Char5, Char6, Char7, Char8, Char9, Char10|Chars]) -->
    [Char1],
    {char_code('\\', Char1)},
    [Char2],
    {char_code('U', Char2)},
    hex_char(Char3),
    hex_char(Char4),
    hex_char(Char5),
    hex_char(Char6),
    hex_char(Char7),
    hex_char(Char8),
    hex_char(Char9),
    hex_char(Char10),
    quoted_chars(Quote, Chars).
quoted_chars(Quote, [Char1, Char2|Chars]) -->
    [Char1],
    {char_code('\\', Char1)},
    [Char2],
    quoted_chars(Quote, Chars).
quoted_chars(Quote, [Char, Char|Chars]) -->
    [Char],
    {char_code(Quote, Char)},
    [Char],
    quoted_chars(Quote, Chars).
quoted_chars(Quote, []) -->
    [Char],
    {char_code(Quote, Char)}.

%! token(Token:compound)
% Match a single token.
token(comment(line, Comment)) -->
    `%`,
	string_without(`\n`, Chars),
    !,
    {atom_chars(Comment, Chars)}.
token(comment(block, Comment)) -->
    `/`,
    `*`,
	string(Chars),
    `*`,
    `/`,
    !,
    {atom_chars(Comment, Chars)}.
token(quoted(back, String)) -->
    [Char],
    {char_code('`', Char)},
	quoted_chars('`', Chars),
    !,
    {atom_chars(String, Chars)}.
token(quoted(double, String)) -->
    [Char],
    {char_code('"', Char)},
	quoted_chars('"', Chars),
    !,
    {atom_chars(String, Chars)}.
token(quoted(single, String)) -->
    [Char],
    {char_code('\'', Char)},
	quoted_chars('\'', Chars),
    !,
    {atom_chars(String, Chars)}.
token(value(binary, Binary)) -->
    `0b`,
    base_digits(Chars, 2),
    !,
    {
        append(`0b`, Chars, BinaryChars),
        atom_chars(Binary, BinaryChars)
    }.
token(value(octal, Octal)) -->
    `0o`,
    base_digits(Chars, 8),
    !,
    {
        append(`0o`, Chars, OctalChars),
        atom_chars(Octal, OctalChars)
    }.
token(value(hex, Hex)) -->
    `0x`,
    hex_chars(Chars),
    !,
    {
        append(`0x`, Chars, HexChars),
        atom_chars(Hex, HexChars)
    }.
token(value(float, PosFloat)) -->
    float_digits(Digits),
    {atom_chars(PosFloat, Digits)}.
token(value(float, NegFloat)) -->
    `-`,
    float_digits(Digits),
    {
        char_code('-', Sign),
        atom_chars(NegFloat, [Sign|Digits])
    }.
token(value(int, PosInt)) -->
    base_digits(Digits, 10),
    {atom_chars(PosInt, Digits)}.
token(value(int, NegInt)) -->
    `-`,
    base_digits(Digits, 10),
    {
        char_code('-', Sign),
        atom_chars(NegInt, [Sign|Digits])
    }.
token(mark(Mark)) -->
    char(Punct, punct),
    !,
    {atom_chars(Mark, [Punct])}.
token(lower(Lower)) -->
    char(Char, lower),
    chars(Chars, csym),
    !,
    {atom_chars(Lower, [Char|Chars])}.
token(upper(Upper)) -->
    char(Char, csymf),
    chars(Chars, csym),
    !,
    {atom_chars(Upper, [Char|Chars])}.

%! float_digits(FloatDigits:codes)
% Parse a floating point except the sign.
float_digits(FloatDigits) -->
    base_digits(WholeDigits, 10),
    `.`,
    base_digits(FracDigits, 10),
    exponent(ExpDigits),
    {
        char_code('.', Point),
        flatten([WholeDigits, Point, FracDigits, ExpDigits], FloatDigits)
    }.

%! exponent(ExpDigits:codes)
% Return a floating-point exponent as a list of codes.
exponent(ExpDigits) -->
    [Char1],
    {
        char_code('e', Char1);
        char_code('E', Char1)
    },
    [Char2],
    {
        char_code('+', Char2);
        char_code('-', Char2)
    },
    base_digits(Exponent, 10),
    {ExpDigits = [Char1, Char2|Exponent]}.
exponent([]) -->
    [].

%! char(Char:code)
% Match a single character code.
char(Char, Type) -->
    [Char],
    {char_type(Char, Type)}.

%! hex_char(Char:code)
% Match a hexadecimal character code.
hex_char(Char) -->
    [Char],
    {char_type(Char, xdigit(_))}.

%! hex_chars(Chars:codes)
% Match and lowercase a list of one or more hexadecimal character codes.
hex_chars([Char|Chars]) -->
    hex_char(Char),
    hex_chars(Chars).
hex_chars([Char]) -->
    hex_char(Char).
