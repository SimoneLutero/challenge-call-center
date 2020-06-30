-module(call_center_client).

-export([connect/0, disconnect/0]).
-export([login/1, logout/1]).
-export([send_message/2]).

%TO DO
%Move the identification from Username to Socket

connect() ->
    sockclient:connect().

login(Username) ->
    sockclient:send_create_session(Username).

logout(Username) ->
    sockclient:send_close_session(Username).

disconnect() ->
    sockclient:disconnect().

send_message(Username, Message) ->
    sockclient:send_message(Username, Message).


