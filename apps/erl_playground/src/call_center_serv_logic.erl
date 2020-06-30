-module(call_center_serv_logic).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([handle_client/1, handle_client/2, operator/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

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

handle_client(State) ->
    send_response_message(show_menu(), State),
    receive
        {<<"1">>} ->
            send_response_message(get_weather_forecast(), State),
            handle_client(State);

        {<<"2">>} ->
            send_response_message(get_joke_of_the_day(), State),
            handle_client(State);

        {<<"3">>} ->
            send_response_message(pid_to_list(self()), State),
            handle_client(State);

        {<<"4">>} ->
            PidOperator = spawn(?MODULE, operator, [self(), 0, State]),
            PidOperator ! {welcome},
            handle_client(State, PidOperator);

        {exit, Reason} ->
            exit(Reason);

        {_} ->
            send_response_message(show_menu(), State),            
            handle_client(State)
    end.

handle_client(State, PidOperator) ->
    receive
        {MessageForOperator} ->
            case process_info(PidOperator) of
                undefined ->
                    send_response_message("Conversation with operator is already terminated", State);
                _else ->
                    PidOperator ! {MessageForOperator},
                    handle_client(State, PidOperator)
            end;
        {PidOperator, terminated} ->
            send_response_message("Conversation with operator is terminated", State),
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

operator(FatherPid, Count, State)
    when Count < 4 ->
    receive
        {Message} when is_list(Message) ->
            send_response_message("This process Pid is " ++ pid_to_list(self()), State),
            operator(FatherPid, Count + 1, State);

        {Message} when is_integer(Message) ->
            Feedback = case even(Message) of
                true ->
                    "The number is even";
                false ->
                    "The number is odd"
            end,
            send_response_message(Feedback, State),
            operator(FatherPid, Count + 1, State);

        {welcome} ->
            send_response_message("Hi, I'm the operator " ++ pid_to_list(self()) ++ ", how can I help you?", State),
            operator(FatherPid, Count + 1, State);

        {_} ->
            send_response_message("I don't understand", State),
            operator(FatherPid, Count + 1, State)

    after
        10000 ->
            send_response_message("Time's up, see you soon", State),
            FatherPid ! {self(), terminated},
            exit(normal)
    end;

operator(FatherPid, Count, State)
    when Count > 3 ->
    send_response_message("Max number of questions reached, see you soon", State),
    FatherPid ! {self(), terminated},
    exit(normal).
            
even(X) when X >= 0 -> (X band 1) == 0.
