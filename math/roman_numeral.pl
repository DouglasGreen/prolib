%%
% <module> Roman numerals
%
% @author Douglas S. Green
% @license GPL

:- module(roman, [
        convert/2,
        to_arabic/2,
        to_roman/2
    ]
).

%! convert(?Arabic:int, ?Roman:int) is multi
% Two-way conversion or counting.
convert(Arabic, Roman) :-
    var(Roman),
    between(1, 4999, Arabic),
    to_roman(Arabic, Roman).
convert(Arabic, Roman) :-
    atom(Roman),
    to_arabic(Roman, Arabic).

%! to_arabic(+Roman:atom, -Arabic:int) is det
% Convert a Roman numeral to an Arabic number.
to_arabic(Roman, Arabic) :-
    atom_chars(Roman, Chars),
    roman_chars(Chars, Arabic).

%! to_roman(-Arabic:int, +Roman:atom) is det
% Convert an Arabic number to a Roman numeral.
to_roman(Arabic, Roman) :-
    roman_value(Roman, Arabic),
    !.
to_roman(Arabic, Roman) :-
    Arabic > 0,
    roman_value(MaxRoman, MaxArabic),
    MaxArabic < Arabic,
    !,
    OtherArabic is Arabic - MaxArabic,
    to_roman(OtherArabic, OtherRoman),
    atom_concat(MaxRoman, OtherRoman, Roman).

%! roman_chars(+Chars:chars, -Arabic:int) is det
% Convert a list of Roman characters to an Arabic number.
roman_chars([Char1, Char2|Chars], Arabic) :-
    roman_value(Char1, Arabic1),
    roman_value(Char2, Arabic2),
    Arabic1 < Arabic2,
    roman_chars(Chars, Arabic3),
    Arabic is Arabic2 - Arabic1 + Arabic3,
    !.
roman_chars([Char|Chars], Arabic) :-
    roman_value(Char, Arabic1),
    roman_chars(Chars, Arabic2),
    Arabic is Arabic1 + Arabic2.
roman_chars([Char], Arabic) :-
    roman_value(Char, Arabic).
roman_chars([], 0).

%! roman_value(?Roman:atom, ?Arabic:int) is nondet
% Look up the value of a Roman number.
roman_value('M', 1000).
roman_value('CM', 900).
roman_value('D', 500).
roman_value('CD', 400).
roman_value('C', 100).
roman_value('L', 50).
roman_value('XL', 40).
roman_value('X', 10).
roman_value('IX', 9).
roman_value('V', 5).
roman_value('IV', 4).
roman_value('I', 1).
