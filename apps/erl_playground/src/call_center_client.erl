-module(call_center_client).

-export([connect/1, disconnect/0]).
-export([send_message/2]).

%TO DO
%Move the identification from Username to Socket

connect(Username) ->
    sockclient:connect(),
    sockclient:send_create_session(Username).

disconnect() ->
    %sockclient:close_session(Username).
    sockclient:disconnect().

send_message(Username, Message) ->
    sockclient:send_message(Username, Message).


