-module(call_center_serv_logic).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([handle_client/1, operator/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport
}).
-type state() :: #state{}.

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
            PidOperator = spawn(?MODULE, operator, [self(), 0, State]),
            PidOperator ! {welcome},

            self() ! {handle_operator, PidOperator};
            % receive
            %     {PidOperator, terminated} ->
            %         handle_client(State);
            %     {MessageForOperator} ->
            %         PidOperator ! {MessageForOperator},
            %         self() ! {<<"4">>, continue}
            % end;

        {handle_operator, PidOperator} ->
            receive
                {PidOperator, terminated} ->
                    Response = #req{
                        type = server_message,
                        server_message_data = #server_message {
                            message = "Conversation with operator is terminated"
                        }
                    },
                    Data = utils:add_envelope(Response),
                    Transport:send(Socket,Data),
                    handle_client(State);
                {MessageForOperator} ->
                    PidOperator ! {MessageForOperator},
                    self() ! {handle_operator, PidOperator}
            end;

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
    "Hi! Press 1 to receive the weather forecast~n2 - Press 2 to receive the joke of the day~n3 - Press 3 to request your call ID~n4 - Press 4 to ask for an operator".

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

        {welcome} ->
            Response = #req{
                type = server_message,
                server_message_data = #server_message {
                    message = "Hi, I'm the operator " ++ pid_to_list(self()) ++ ", how can I help you?"
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
    exit(normal),
    State.

            
even(X) when X >= 0 -> (X band 1) == 0.
