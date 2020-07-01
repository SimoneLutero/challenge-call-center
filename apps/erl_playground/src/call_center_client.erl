-module(call_center_client).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([connect/0, disconnect/1]).
-export([login/1, logout/1]).
-export([send_message/2]).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

% connect to a tcp_server that run sockserv module
connect() ->
    sockclient:connect().

% create a session on the server related to the inserted username
login(Username) ->
    sockclient:send_create_session(Username).

% close the session on the server related to the inserted username
logout(Username) ->
    sockclient:send_close_session(Username).

% close the session and terminate the connection with server
disconnect(Username) ->
    sockclient:send_close_session(Username),
    sockclient:disconnect().

% send the inserted message to the server if the content is an integer
send_message(Username, Message) 
    when is_integer(Message)->
        sockclient:send_message(Username, integer_message, Message);

% send the inserted message to the server if the content is an list
send_message(Username, Message)
    when is_list(Message) ->
        sockclient:send_message(Username, string_message, Message);

% handle wrong message types
send_message(_, _) ->
    io:fwrite("Type not handled ~n"),
    error.

