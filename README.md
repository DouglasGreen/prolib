# prolib
Prolog library modules

## Lexer

The lexer ignores white space and produces labeled tokens. It has different predicates for different token types. Here
are a couple of examples.

### `lexer:alnum_tokens/3`

This predicate is intended for parsing HTML or XML where letters are mixed with numbers.

```
?- phrase(lexer:alnum_tokens(Ts), `<div id="6c7018bdffebedadd3cba91109b5531d">`).
Ts = [mark(<), alnum(div), alnum(id), mark(=), mark('"'), alnum('6c7018bdffebedadd3cba91109b5531d'),
    mark('"'), mark(>)] .

```
### `lexer:word_tokens/3`

This predicate is intended for parsing English text where the distinction between upper and lowercase words is important.

```
?- phrase(lexer:word_tokens(Ts), `This is the #1 test.`).
Ts = [word('This', upper), word(is, lower), word(the, lower), mark(#), number('1'),
    word(test, lower), mark('.')] .
```
