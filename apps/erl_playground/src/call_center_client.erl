-module(call_center_client).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([connect/0, disconnect/0]).
-export([login/1, logout/1]).
-export([send_message/2]).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

connect() ->
    sockclient:connect().

login(Username) ->
    sockclient:send_create_session(Username).

logout(Username) ->
    sockclient:send_close_session(Username).

disconnect() ->
    sockclient:disconnect().

send_message(Username, Message) 
    when is_integer(Message)->
        sockclient:send_message(Username, integer_message, Message);

send_message(Username, Message)
    when is_list(Message) ->
        sockclient:send_message(Username, string_message, Message).


