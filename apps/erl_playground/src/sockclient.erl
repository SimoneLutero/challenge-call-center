-module(sockclient).
-behaviour(gen_server).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref([{start_link, 4}]).
-export([connect/0, disconnect/0]).
-export([send_create_session/1, send_close_session/1]).
-export([send_message/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any()
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

start_link() ->
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

-spec connect() -> ok.
connect() ->
    gen_server:call(whereis(?SERVER), connect),
    ok.

-spec disconnect() -> ok.
disconnect() ->
    gen_server:call(whereis(?SERVER), disconnect),
    ok.

-spec send_create_session() -> ok.
send_create_session() ->
    CreateSession = #create_session {
        username = <<"TestUser">>
    },
    gen_server:cast(whereis(?SERVER), {create_session, CreateSession}).

send_create_session(Username) ->
    CreateSession = #create_session {
        username = Username
    },
    gen_server:cast(whereis(?SERVER), {create_session, CreateSession}).

send_close_session(Username) ->
    CloseSession = #close_session {
        username = Username
    },
    gen_server:cast(whereis(?SERVER), {close_session, CloseSession}).

send_message(Username, Message) ->
    ClientMessage = #client_message {
        username = Username,
        message_body = Message
    },
    gen_server:cast(whereis(?SERVER), {client_message, ClientMessage}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init(_ARgs) ->
    lager:info("sockclient init'ed"),
    {ok, #state{}}.

handle_cast({create_session, CreateSession}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = create_session,
        create_session_data = CreateSession
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),

    {noreply, State};

handle_cast({close_session, CloseSession}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = close_session,
        close_session_data = CloseSession
    },
    Data = utils:add_envelope(Req),
    
    gen_tcp:send(Socket, Data),

    {noreply, State};

handle_cast({client_message, ClientMessage}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = client_message,
        client_message_data = ClientMessage
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),

    {noreply, State};

handle_cast(Message, State) ->
    _ = lager:warning("No handle_cast for ~p", [Message]),
    {noreply, State}.

handle_info({tcp_closed, _Port}, State) ->
    {noreply, State#state{socket = undefined}};
handle_info({tcp, _Port, Packet}, State) ->
    Req = utils:open_envelope(Packet),
    State = process_packet(Req, State, utils:unix_timestamp()),
    {noreply, State};
handle_info(Message, State) ->
    _ = lager:warning("No handle_info for~p", [Message]),
    {noreply, State}.

handle_call(connect, _From, State) ->
    {ok, Host} = application:get_env(erl_playground, tcp_host),
    {ok, Port} = application:get_env(erl_playground, tcp_port),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 2}]),

    {reply, normal, State#state{socket = Socket}};
handle_call(disconnect, _From, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    
    gen_tcp:shutdown(Socket, read_write),

    {reply, normal, State};
handle_call(Message, _From, State) ->
    _ = lager:warning("No handle_call for ~p", [Message]),
    {reply, normal, State}.

terminate(Reason, _State) ->
    _ = lager:notice("terminate ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    lager:notice("server sent invalid packet, ignoring"),
    State;
process_packet(#req{ type = Type } = Req, State, _Now)
    when Type =:= server_message ->
    #req{
        server_message_data = #server_message{
            message = Message
        }
    } = Req,
    _ = lager:info("server_message received: ~p", [Message]),
    State.
