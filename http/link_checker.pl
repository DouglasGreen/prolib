%% <module> Link checker */
%
% @author Douglas S. Green
% @license GPL

:- module(link_checker, [
        check_link/3
    ]
).

:- use_module(http).
:- use_module(library(http/http_open)).

%! check_link(+Link:string, -Code:int, -Message:String) is det
% Check a link and return a status or error code and message.
check_link(Link, Code, Message) :-
    catch(
        (
            http_open(Link, _, [status_code(Code)]),
            http_code(Code, Message),
            !
        ),
        Error,
        handle_link_error(Error, Code, Message)
    ).

%! handle_link_error(+Error:compound) is det
% Handle known error types.
handle_link_error(Error, Code, Message) :-
    Error = error(socket_error(_,Message),_),
	Code = -1,
    !.
handle_link_error(_, 0, "Unknown error").
