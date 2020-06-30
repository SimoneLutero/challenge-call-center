-module(utils).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([add_envelope/1, open_envelope/1]).
-export([unix_timestamp/0]).
-export([send_response_message/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec add_envelope(Req :: #req{}) -> Packet :: binary().
add_envelope(Req) ->
    erl_playground_pb:encode_msg(#envelope{
        uncompressed_data = Req
    }).

-spec open_envelope(Packet :: binary()) -> Req :: #req{}.
open_envelope(Packet) ->
    Envelope = erl_playground_pb:decode_msg(Packet, envelope),
    case Envelope of
        #envelope{uncompressed_data = Req} when Req =/= undefined ->
            Req;
        _ ->
            undefined
    end.

-spec unix_timestamp() -> non_neg_integer().
unix_timestamp() ->
    {Msec, Sec, _} = os:timestamp(),
    Msec * 1000000 + Sec.

-spec send_response_message(Message :: binary(), State :: state()) -> state().
send_response_message(Message, State = {ok, #state{socket = Socket, transport = Transport}}) ->
    Response = #req{
        type = server_message,
        server_message_data = #server_message {
            message = Message
        }
    },
    Data = utils:add_envelope(Response),
    Transport:send(Socket,Data),

    State.