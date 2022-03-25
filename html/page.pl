%% <module> Page loader */
%
% @author Douglas S. Green
% @license GPL

:- module(page, [
        load/2,
        scrape/2
    ]
).

:- use_module(library(http/http_open)).

%! scrape(+URL:string, -DOM:DTD)
% Load a URL with error handling.
scrape(URL, DOM) :-
    catch(
        load(URL, DOM), Error, (
            print_message(warning, Error),
            fail
        )
    ),
    !.

%! load(+URL:string, -DOM:DTD)
% Load a URL without error handling.
load(URL, DOM) :-
    setup_call_cleanup(
        http_open(URL, In, [timeout(30)]),
        (
            dtd(html, DTD),
            load_structure(
                stream(In),
                DOM,
                [
                    cdata(string),
                    dialect(sgml),
                    dtd(DTD),
                    shorttag(false),
                    syntax_errors(quiet)
                ]
            )
        ),
        close(In)
    ).
