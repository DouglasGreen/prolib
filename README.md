# prolib
Prolog library modules

## Lexer

The lexer (`lexer.pl`) produces labeled tokens. It has different predicates for different token types. Here are some
examples.

### `lexer:all_tokens/3`

This predicate is intended for parsing line-oriented languages where space, tab, and new line matter. The other
predicates ignore white space.

```
?- phrase(lexer:all_tokens(Ts), `    \t\tprint.\n`).
Ts = [space(4), tab(2), alpha(print), mark('.'), nl(1)] .
```

### `lexer:alnum_tokens/3`

This predicate is intended for parsing HTML or XML where letters are mixed with numbers.

```
?- phrase(lexer:alnum_tokens(Ts), `<div id="6c7018bdffebedadd3cba91109b5531d">`).
Ts = [mark(<), alnum(div), alnum(id), mark(=), mark('"'), alnum('6c7018bdffebedadd3cba91109b5531d'),
    mark('"'), mark(>)] .
```

### `lexer:alpha_tokens/3`

This predicate is like `alnum_tokens/3` but it separates the alpha from the numeric.

```
?- phrase(lexer:alpha_tokens(Ts), `Easy as abc123!`).
Ts = [alpha('Easy'), alpha(as), alpha(abc), number('123'), mark(!)] .
```

### `lexer:word_tokens/3`

This predicate is intended for parsing English text where the distinction between upper and lowercase words is important.

```
?- phrase(lexer:word_tokens(Ts), `This is the #1 test.`).
Ts = [word('This', upper), word(is, lower), word(the, lower), mark(#), number('1'),
    word(test, lower), mark('.')] .
```
