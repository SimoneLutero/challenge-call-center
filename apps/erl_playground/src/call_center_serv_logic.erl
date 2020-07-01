-module(call_center_serv_logic).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([handle_client/1, handle_client/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(OPERATOR_MODULE, call_center_operator_logic).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

handle_client(State) ->
    utils:send_response_message(show_menu(), State),
    receive
        {1} ->
            utils:send_response_message(get_weather_forecast(), State),
            handle_client(State);

        {2} ->
            utils:send_response_message(get_joke_of_the_day(), State),
            handle_client(State);

        {3} ->
            utils:send_response_message(pid_to_list(self()), State),
            handle_client(State);

        {4} ->
            PidOperator = spawn(?OPERATOR_MODULE, operator, [self(), State]),
            PidOperator ! {welcome},
            handle_client(State, PidOperator);

        {exit, Reason} ->
            exit(Reason);

        {_} ->           
            handle_client(State)
    end.

handle_client(State, PidOperator) ->
    receive
        {MessageForOperator} ->
            case process_info(PidOperator) of
                undefined ->
                    utils:send_response_message("Conversation with operator is already terminated", State);
                _else ->
                    PidOperator ! {MessageForOperator},
                    handle_client(State, PidOperator)
            end;
        {PidOperator, terminated} ->
            utils:send_response_message("Conversation with operator is terminated", State),
            handle_client(State)
    end.

show_menu() ->
    "Hi! Send 1 to receive the weather forecast for tomorrow 2 - Send 2 to receive a random joke 3 - Send 3 to request your call ID 4 - Send 4 to ask for an operator".

get_weather_forecast() ->
    case weather_forecast:get_weather() of
        {Forecast} ->
            Forecast;
        {error, _} ->
            "Error during API call"
    end.

get_joke_of_the_day() ->
    case joke_of_the_day:get_joke() of
        {Joke} -> 
            Joke;
        {error, _} ->
            "Error during API call"
    end.