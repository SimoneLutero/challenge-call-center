-module(sockserv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]). -ignore_xref([{start_link, 4}]).
-export([start/0]).
-export([handle_client/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/4]). -ignore_xref([{init, 4}]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start() ->
    {ok, Port} = application:get_env(erl_playground, tcp_port),
    {ok, MaxConnections} = application:get_env(erl_playground, max_connections),

    TcpOptions = [
        {backlog, 100}
    ],

    % Start a tcp_listener
    % sockserv_tcp is the name
    % ranch_tcp is the transport handler
    % options[
    %   {port, Port} is the port where to listen
    %   {num_acceptors, 100} is the number of acceptor processes. Their role is to accept connections and spawn a connection process for every new connection.
    %   {backlog, 100} is the max number of connections in queue. They go in queue after max_connections connections (default 1024).
    % ]
    % sockserv is the protocol handler.

    {ok, _} = ranch:start_listener(
        sockserv_tcp,
        ranch_tcp,
        [{port, Port},
        {num_acceptors, 100}] ++ TcpOptions,
        sockserv,
        [none]
    ),

    ranch:set_max_connections(sockserv_tcp, MaxConnections),
    lager:info("server listening on tcp port ~p", [Port]),
    ok.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Definitions
%% ------------------------------------------------------------------

init(Ref, Socket, Transport, [_ProxyProtocol]) ->
    lager:info("sockserv init'ed ~p",[Socket]),

    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    Opts = [{packet, 2}, {packet_size, 16384}, {active, once}, {nodelay, true}],
    _ = Transport:setopts(Socket, Opts),

    State = {ok, #state{
        socket = Socket,
        transport = Transport
    }},

    gen_server:enter_loop(?MODULE, [], State).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

handle_cast(Message, State) ->
    _ = lager:notice("unknown handle_cast ~p", [Message]),
    {noreply, State}.

handle_info({tcp, _Port, <<>>}, State) ->
    _ = lager:notice("empty handle_info state: ~p", [State]),
    {noreply, State};
handle_info({tcp, _Port, Packet}, State = {ok, #state{socket = Socket}}) ->
    Req = utils:open_envelope(Packet),

    State = process_packet(Req, State, utils:unix_timestamp()),
    ok = inet:setopts(Socket, [{active, once}]),

    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};
handle_info(Message, State) ->
    _ = lager:notice("unknown handle_info ~p", [Message]),
    {noreply, State}.

handle_call(Message, _From, State) ->
    _ = lager:notice("unknown handle_call ~p", [Message]),
    {noreply, State}.

terminate(normal, _State) ->
    _ = lager:info("Goodbye!"),
    ok;
terminate(Reason, _State) ->
    _ = lager:notice("No terminate for ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    _ = lager:notice("client sent invalid packet, ignoring ~p",[State]),
    State;
process_packet(#req{ type = Type } = Req, State = {ok, #state{socket = Socket, transport = Transport}}, _Now)
    when Type =:= create_session ->
    #req{
        create_session_data = #create_session {
            username = UsernameBinary
        }
    } = Req,
    _ = lager:info("create_session received from ~p", [UsernameBinary]),

    UsernameAtom = binary_to_atom(UsernameBinary, utf8),

    %spawn a process with client name
    case whereis(UsernameAtom) of 
        undefined ->
            register(UsernameAtom, spawn(?MODULE, handle_client, [State])),
            Feedback = "session created";
        _else ->
            Feedback = "session already exists"
    end,

    Response = #req{
        type = server_message,
        server_message_data = #server_message {
            message = Feedback
        }
    },
    Data = utils:add_envelope(Response),
    Transport:send(Socket,Data),

    UsernameAtom ! {normal},

    State;

process_packet(#req{ type = Type } = Req, State = {ok, #state{socket = Socket, transport = Transport}}, _Now)
    when Type =:= close_session ->
    #req{
        close_session_data = #create_session {
            username = UsernameBinary
        }
    } = Req,
    _ = lager:info("close_session received from ~p", [UsernameBinary]),

    UsernameAtom = binary_to_atom(UsernameBinary, utf8),

    %spawn a process with client name
    case whereis(UsernameAtom) of 
        undefined ->
            Feedback = "no session exists";
        _else ->
            UsernameAtom ! {exit, normal},
            Feedback = "session closed"
    end,

    Response = #req{
        type = server_message,
        server_message_data = #server_message {
            message = Feedback
        }
    },
    Data = utils:add_envelope(Response),
    Transport:send(Socket,Data),

    State;

process_packet(#req{ type = Type } = Req, State, _Now)
    when Type =:= client_message ->
    #req{
        client_message_data = #client_message {
            username = UsernameBinary,
            message_body = MessageBody
        }
    } = Req,
    _ = lager:info("client_message received from ~p", [UsernameBinary]),

    UsernameAtom = binary_to_atom(UsernameBinary, utf8), 
    
    case whereis(UsernameAtom) of 
        undefined ->
            _ = lager:info("session not found, creating a session"),
            process_packet(#req{ type = create_session, create_session_data = UsernameBinary}, State, _Now);
        _else ->
            ok
    end,

    %send data to process with client name
    UsernameAtom ! {MessageBody},

    State.

handle_client(State = {ok, #state{socket = Socket, transport = Transport}}) ->
    receive
        {<<"1">>} ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = get_weather_forecast()
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            handle_client(State);

        {<<"2">>} ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = get_joke_of_the_day()
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            handle_client(State);

        {<<"3">>} ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = pid_to_list(self())
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            handle_client(State);

        {<<"4">>} ->
            PidOperator = spawn(sockserv, operator, [self(), 0, State]),

            receive
                {PidOperator, terminated} ->
                    handle_client(State)
            end,

            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = "Conversation with operator is terminated"
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            handle_client(State);

        {exit, Reason} ->
            exit(Reason);

        {_} ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = show_menu()
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            
            handle_client(State)
    end.

show_menu() ->
    "Hi! Press 1 to receive the weather forecast\n2 - Press 2 to receive the joke of the day\n3 - Press 3 to request your call ID\n4 - Press 4 to ask for an operator".

get_weather_forecast() ->
    List = ["Sunny","Rainy","Cloudy"],
	Index = rand:uniform(length(List)),
	lists:nth(Index, List).

get_joke_of_the_day() ->
    List = ["Funny Joke 1","Funny Joke 2","Funny Joke 3"],
	Index = rand:uniform(length(List)),
	lists:nth(Index, List).

operator(FatherPid, Count, State = {ok, #state{socket = Socket, transport = Transport}})
    when Count < 3 ->
    receive
        {Message} when is_list(Message) ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = "This process Pid is " ++ pid_to_list(self())
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            operator(FatherPid, Count + 1, State);

        {Message} when is_integer(Message) ->
            case even(Message) of
                true ->
                    Feedback = "The number is even";
                false ->
                    Feedback = "The number is odd"
            end,
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = Feedback
                }
            },            
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            operator(FatherPid, Count + 1, State);

        {_} ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = "I don't understand"
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            operator(FatherPid, Count + 1, State)

    after
        10000 ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = "Time's up, see you soon"
                }
            },
            Data = utils:add_envelope(Response),
            Transport:send(Socket,Data),
            FatherPid ! {self(), terminated},
            exit(normal)
    end;

operator(FatherPid, Count, State = {ok, #state{socket = Socket, transport = Transport}})
    when Count > 3 ->
    Response = #req{
        type = server_message,
        server_message_data = #server_message {
            message = "Max number of questions reached, see you soon"
        }
    },
    Data = utils:add_envelope(Response),
    Transport:send(Socket,Data),
    FatherPid ! {self(), terminated},
    exit(normal).

            
even(X) when X >= 0 -> (X band 1) == 0.

% odd(X) when X > 0 -> not even(X).